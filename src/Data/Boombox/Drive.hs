{-# LANGUAGE Rank2Types, LambdaCase, BangPatterns, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Data.Boombox.Drive where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Co
import Control.Comonad

data Drive w e s m a = Done ![s] a
  | Partial (s -> Drive w e s m a)
  | Failed ![s] !e
  | Eff (CoT w m (Drive w e s m a))
  deriving Functor

instance Functor w => Applicative (Drive w e s m) where
  pure = return
  (<*>) = ap

instance Functor w => Monad (Drive w e s m) where
  return a = Done [] a
  m >>= k = go m where
    go (Done s a) = supplyDrive s (k a)
    go (Partial f) = Partial (go . f)
    go (Failed s e) = Failed s e
    go (Eff f) = Eff (fmap (>>=k) f)

supplyDrive :: Functor w => [s] -> Drive w e s m a -> Drive w e s m a
supplyDrive [] p = p
supplyDrive (x:xs) (Partial f) = supplyDrive xs (f x)
supplyDrive xs (Done s a) = Done (s ++ xs) a
supplyDrive xs (Failed s e) = Failed (s ++ xs) e
supplyDrive xs (Eff m) = Eff $ supplyDrive xs <$> m

catchDrive :: Functor w => (e -> Drive w e' s m a) -> Drive w e s m a -> Drive w e' s m a
catchDrive k (Failed xs e) = supplyDrive xs (k e)
catchDrive _ (Done s a) = Done s a
catchDrive k (Partial f) = Partial (catchDrive k . f)
catchDrive k (Eff m) = Eff $ fmap (catchDrive k) m

newtype PlayerT w e s m a = PlayerT { unPlayerT :: forall r. [s]
    -> ([s] -> e -> Drive w e s m r)
    -> ([s] -> a -> Drive w e s m r)
    -> Drive w e s m r }

instance Functor (PlayerT w e s m) where
  fmap = liftM

instance Applicative (PlayerT w e s m) where
  pure = return
  (<*>) = ap

instance Monad (PlayerT w e s m) where
  return a = PlayerT $ \s _ cs -> cs s a
  m >>= k = PlayerT $ \s ce cs -> unPlayerT m s ce $ \s' a -> unPlayerT (k a) s' ce cs

instance Comonad w => MonadTrans (PlayerT w e s) where
  lift m = PlayerT $ \s _ cs -> Eff $ fmap (cs s) (lift m)

instance (Comonad w, MonadIO m) => MonadIO (PlayerT w e s m) where
  liftIO m = PlayerT $ \s _ cs -> Eff $ fmap (cs s) (liftIO m)

instance (Monoid e) => Alternative (PlayerT w e s m) where
  empty = failed mempty
  p <|> q = PlayerT $ \s ce cs -> unPlayerT p s (\s' e -> unPlayerT q s' (\s'' -> ce s'' . mappend e) cs) cs

instance Monoid a => Monoid (PlayerT w e s m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

runPlayerT :: PlayerT w e s m a -> Drive w e s m a
runPlayerT m = unPlayerT m [] Failed Done

failed :: e -> PlayerT w e s m a
failed e = PlayerT $ \s ce _ -> ce s e

consume :: PlayerT w e s m [s]
consume = PlayerT $ \s ce cs -> Partial $ \x -> unPlayerT consume s ce $ \l xs -> cs l (x : xs)

try :: Functor w => PlayerT w e s m a -> PlayerT w e s m a
try pl = PlayerT $ \s ce cs -> go ce (reverse s) (unPlayerT pl s Failed cs) where
  go ce xs (Partial f) = Partial (\x -> go ce (x : xs) (f x))
  go _ _ (Done s a) = Done s a
  go ce xs (Eff m) = Eff $ fmap (go ce xs) m
  go ce xs (Failed _ e) = ce (reverse xs) e

await :: PlayerT w e s m s
await = PlayerT $ \s _ cs -> case s of
  (x:xs) -> cs xs x
  [] -> Partial $ \s' -> cs [] s'

leftover :: [s] -> PlayerT w e s m ()
leftover ss = PlayerT $ \s _ cs -> cs (s ++ ss) ()

catchPlayerT :: PlayerT w e s m a -> (e -> PlayerT w e s m a) -> PlayerT w e s m a
catchPlayerT m k = PlayerT $ \s ce cs -> unPlayerT m s (\s' e -> unPlayerT (k e) s' ce cs) cs

lookAhead :: Functor w => PlayerT w e s m a -> PlayerT w e s m a
lookAhead pl = PlayerT $ \s ce cs -> go ce cs (reverse s) (unPlayerT pl s Failed Done) where
  go ce cs xs (Partial f) = Partial (\x -> go ce cs (x : xs) (f x))
  go _ cs xs (Done _ a) = cs (reverse xs) a
  go ce cs xs (Eff m) = Eff $ fmap (go ce cs xs) m
  go ce _ xs (Failed _ e) = ce (reverse xs) e
