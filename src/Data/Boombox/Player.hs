{-# LANGUAGE Rank2Types, LambdaCase, BangPatterns, DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Data.Boombox.Player where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Comonad

data Drive w s m a = Done a
  | Partial (s -> Drive w s m a)
  | Leftover s (Drive w s m a)
  | Eff (m (Drive w s m a))
  | Cont (forall r. w (Drive w s m a -> r) -> r)

newtype PlayerT w s m a = PlayerT { unPlayerT :: forall r. (a -> Drive w s m r) -> Drive w s m r }

instance Functor (PlayerT w s m) where
  fmap f m = PlayerT $ \cs -> unPlayerT m (cs . f)

instance Applicative (PlayerT w s m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (PlayerT w s m) where
  return a = PlayerT $ \cs -> cs a
  {-# INLINE return #-}
  m >>= k = PlayerT $ \cs -> unPlayerT m $ \a -> unPlayerT (k a) cs
  {-# INLINE (>>=) #-}

instance MonadTrans (PlayerT w s) where
  lift m = PlayerT $ \cs -> Eff $ fmap cs m
  {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (PlayerT w s m) where
  liftIO m = PlayerT $ \cs -> Eff $ fmap cs (liftIO m)
  {-# INLINE liftIO #-}

instance Monoid a => Monoid (PlayerT w s m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

runPlayerT :: PlayerT w s m a -> Drive w s m a
runPlayerT m = unPlayerT m Done
{-# INLINE runPlayerT #-}

-- | Send a control signal.
control :: Comonad w => (forall a. w a -> (a, b)) -> PlayerT w s m b
control k = PlayerT $ \cs -> Cont $ \wcont -> case k wcont of
  (cont, b) -> cont (cs b)

await :: PlayerT w s m s
await = PlayerT Partial

leftover :: s -> PlayerT w s m ()
leftover s = PlayerT $ \cs -> Leftover s (cs ())

{-
-- | Try to run the given action. If the action failed, the input stream will be unconsumed.
try :: Functor w => PlayerT w e s m a -> PlayerT w e s m a
try pl = PlayerT $ \s ce cs -> go ce (reverse s) (unPlayerT pl s Failed cs) where
  go ce xs (Partial f) = Partial (\x -> go ce (x : xs) (f x))
  go _ _ (Done s a) = Done s a
  go ce xs (Eff m) = Eff $ fmap (go ce xs) m
  go ce xs (Failed _ e) = ce (reverse xs) e

-- | Wait for an input.
await :: PlayerT w e s m s
await = PlayerT $ \s _ cs -> case s of
  (x:xs) -> cs xs x
  [] -> Partial $ cs []

-- | Put a leftover input.

catchPlayerT :: PlayerT w e s m a -> (e -> PlayerT w e s m a) -> PlayerT w e s m a
catchPlayerT m k = PlayerT $ \s ce cs -> unPlayerT m s (\s' e -> unPlayerT (k e) s' ce cs) cs

-- | Run a 'PlayerT' action without consuming any input.
lookAhead :: Functor w => PlayerT w e s m a -> PlayerT w e s m a
lookAhead pl = PlayerT $ \s ce cs -> go ce cs (reverse s) (unPlayerT pl s Failed Done) where
  go ce cs xs (Partial f) = Partial (\x -> go ce cs (x : xs) (f x))
  go _ cs xs (Done _ a) = cs (reverse xs) a
  go ce cs xs (Eff m) = Eff $ fmap (go ce cs xs) m
  go ce _ xs (Failed _ e) = ce (reverse xs) e
-}