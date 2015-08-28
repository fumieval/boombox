{-# LANGUAGE Rank2Types, LambdaCase, BangPatterns, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Data.Boombox.Drive where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Proxy

data Drive e s m a = Done [s] a
  | Partial (Drive e s m a) (s -> Drive e s m a)
  | Failed [s] !e
  | Eff (m (Drive e s m a))
  deriving Functor

instance Functor m => Applicative (Drive e s m) where
  pure = return
  (<*>) = ap

instance Functor m => Monad (Drive e s m) where
  return a = Done [] a
  m >>= k = go m where
    go (Done s a) = supplyDrive s (k a)
    go (Partial e f) = Partial (go e) (go . f)
    go (Failed s e) = Failed s e
    go (Eff f) = Eff (fmap (>>=k) f)

unDrive :: Monad m => ([s] -> a -> m r) -> (m r -> (s -> m r) -> m r) -> ([s] -> e -> m r) -> Drive e s m a -> m r
unDrive done part failed = go where
  go (Done s a) = done s a
  go (Partial e f) = part (go e) (go . f)
  go (Failed s e) = failed s e
  go (Eff m) = m >>= go

supplyDrive :: Functor m => [s] -> Drive e s m a -> Drive e s m a
supplyDrive [] p = p
supplyDrive (x:xs) (Partial _ f) = supplyDrive xs (f x)
supplyDrive xs (Done s a) = Done (s ++ xs) a
supplyDrive xs (Failed s e) = Failed (s ++ xs) e
supplyDrive xs (Eff m) = Eff $ supplyDrive xs <$> m

finishDrive :: Monad m => Drive e s m a -> m (Either e a)
finishDrive (Done _ a) = return (Right a)
finishDrive (Partial s _) = finishDrive s
finishDrive (Failed _ e) = return (Left e)
finishDrive (Eff m) = m >>= finishDrive

newtype PlayerT e s m a = PlayerT { unPlayerT :: forall r. [s]
    -> (e -> Drive e s m r)
    -> ([s] -> a -> Drive e s m r)
    -> Drive e s m r }

instance Functor (PlayerT e s m) where
  fmap = liftM

instance Applicative (PlayerT e s m) where
  pure = return
  (<*>) = ap

instance Monad (PlayerT e s m) where
  return a = PlayerT $ \s _ cs -> cs s a
  m >>= k = PlayerT $ \s ce cs -> unPlayerT m s ce $ \s' a -> unPlayerT (k a) s' ce cs

instance MonadTrans (PlayerT e s) where
  lift m = PlayerT $ \s _ cs -> Eff $ liftM (cs s) m

failPlayer :: e -> PlayerT e s m a
failPlayer e = PlayerT $ \_ ce _ -> ce e

instance (Monoid e, Functor m) => Alternative (PlayerT e s m) where
  empty = failPlayer mempty
  p <|> q = PlayerT $ \s ce cs -> unPlayerT p s (\e -> unPlayerT q s (\f -> ce $ mappend e f) cs) cs

runPlayerT :: PlayerT e s m a -> Drive e s m a
runPlayerT m = unPlayerT m [] (Failed []) Done

await :: AsError EndOfStream e => PlayerT e s m s
await = PlayerT $ \s ce cs -> case s of
  (x:xs) -> cs xs x
  [] -> Partial (ce endOfStream) $ \s' -> cs [] s'

peek :: AsError EndOfStream e => PlayerT e s m s
peek = PlayerT $ \s ce cs -> case s of
  xs@(x:_) -> cs xs x
  [] -> Partial (ce endOfStream) $ \s' -> cs [s'] s'

consume :: PlayerT e s m [s]
consume = PlayerT $ \s ce cs -> Partial (cs [] s) (\x -> unPlayerT consume s ce $ \l xs -> cs l (x : xs))

class AsError t e where
  fromError :: proxy t -> e

data EndOfStream

endOfStream :: AsError EndOfStream e => e
endOfStream = fromError (Proxy :: Proxy EndOfStream)

instance (c ~ Char) => AsError EndOfStream [c] where
  fromError _ = "End of stream"

testPlayerT :: [s] -> PlayerT String s IO a -> IO a
testPlayerT s m = (>>=either fail return) $ finishDrive $ supplyDrive s $ runPlayerT m

{-
consuming :: (Semigroup s, Functor m)
  => (s -> m (Maybe (a, s)))
  -> PlayerT e s m a
consuming m = PlayerT $ \s cont -> Eff $ flip fmap (m s) $ \case
  Nothing -> Partial (\t -> let !u = s <> t in unPlayerT (consuming m) u cont)
  Just (a, s') -> cont s' a

-}
