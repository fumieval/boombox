{-# LANGUAGE Rank2Types, LambdaCase, BangPatterns, DeriveFunctor #-}
module Data.Boombox.Drive where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Semigroup

data Drive e s m a = Done [s] a
  | Partial (s -> Drive e s m a)
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
    go (Partial f) = Partial $ go . f
    go (Failed s e) = Failed s e
    go (Eff f) = Eff (fmap (>>=k) f)

unDrive :: Monad m => ([s] -> a -> m r) -> ((s -> m r) -> m r) -> ([s] -> e -> m r) -> Drive e s m a -> m r
unDrive done part failed = go where
  go (Done s a) = done s a
  go (Partial f) = part $ go . f
  go (Failed s e) = failed s e
  go (Eff m) = m >>= go

supplyDrive :: Functor m => [s] -> Drive e s m a -> Drive e s m a
supplyDrive [] p = p
supplyDrive (x:xs) (Partial f) = supplyDrive xs (f x)
supplyDrive xs (Done s a) = Done (s ++ xs) a
supplyDrive xs (Failed s e) = Failed (s ++ xs) e
supplyDrive xs (Eff m) = Eff $ supplyDrive xs <$> m

newtype PlayerT e s m a = PlayerT { unPlayerT :: forall r. s -> (s -> a -> Drive e s m r) -> Drive e s m r }

instance Functor (PlayerT e s m) where
  fmap = liftM

instance Applicative (PlayerT e s m) where
  pure = return
  (<*>) = ap

instance Monad (PlayerT e s m) where
  return a = PlayerT $ \s cont -> cont s a
  m >>= k = PlayerT $ \s cont -> unPlayerT m s $ \s' a -> unPlayerT (k a) s' cont

instance MonadTrans (PlayerT e s) where
  lift m = PlayerT $ \s cont -> Eff $ liftM (cont s) m

failPlayer :: e -> PlayerT e s m a
failPlayer e = PlayerT $ \s _ -> Failed [s] e

instance (Monoid e, Functor m) => Alternative (PlayerT e s m) where
  empty = failPlayer mempty
  p <|> q = PlayerT $ \s cont -> go [] (unPlayerT p s cont) (unPlayerT q s cont) where
    go _ (Done s a) _ = Done s a
    go ss (Partial f) t = Partial $ \s -> go (s : ss) (f s) t
    go ss (Eff m) t = Eff $ fmap (go ss `flip` t) m
    go ss (Failed _ e) t = run e (reverse ss) t
    run e xs (Failed ss f) = Failed (ss ++ xs) (mappend e f)
    run e (x:xs) (Partial f) = run e xs (f x)
    run e [] (Partial f) = Partial (run e [] . f)
    run e xs (Eff m) = Eff $ fmap (run e xs) m
    run _ xs (Done ss a) = Done (ss ++ xs) a

runPlayerT :: PlayerT e s m a -> Drive e s m a
runPlayerT m = Partial $ \i -> unPlayerT m i (Done . pure)

consuming :: (Semigroup s, Functor m)
  => (s -> m (Maybe (a, s)))
  -> PlayerT e s m a
consuming m = PlayerT $ \s cont -> Eff $ flip fmap (m s) $ \case
  Nothing -> Partial (\t -> let !u = s <> t in unPlayerT (consuming m) u cont)
  Just (a, s') -> cont s' a

await :: PlayerT e s m s
await = PlayerT $ \s cont -> Partial $ \t -> cont t s
