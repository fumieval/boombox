{-# LANGUAGE Rank2Types, LambdaCase, BangPatterns, DeriveFunctor, ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Data.Boombox.Player where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative

data Drive w s m a = Done a
  | Partial (s -> Drive w s m a)
  | Leftover s (Drive w s m a)
  | Eff (m (Drive w s m a))
  | Cont (forall r. w (Drive w s m a -> r) -> r)

-- | @'Player' w s m a@ is a monadic consumer of a stream of @s@.
-- 'Player' may send a control signal parameterized by @w@; the control surface of the producer
-- (usually 'Tape') should match it.
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
  m >>= k = PlayerT $ \cs -> unPlayerT m $ \a -> unPlayerT (k a) cs

instance MonadTrans (PlayerT w s) where
  lift m = PlayerT $ \cs -> Eff $ fmap cs m

instance (MonadIO m) => MonadIO (PlayerT w s m) where
  liftIO m = PlayerT $ \cs -> Eff $ fmap cs (liftIO m)

instance Monoid a => Monoid (PlayerT w s m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

runPlayerT :: PlayerT w s m a -> Drive w s m a
runPlayerT m = unPlayerT m Done

-- | Send a control signal.
control :: (forall a. w a -> (a, PlayerT w s m b)) -> PlayerT w s m b
control k = PlayerT $ \cs -> Cont $ \wcont -> case k wcont of
  (cont, b) -> cont $ unPlayerT b cs

-- | Consume a value.
await :: PlayerT w s m s
await = PlayerT Partial
{-# INLINABLE await #-}

-- | Push a leftover back.
leftover :: s -> PlayerT w s m ()
leftover s = PlayerT $ \cs -> Leftover s (cs ())
{-# INLINABLE leftover #-}

-- | Put some leftovers.
leftovers :: Foldable f => f s -> PlayerT w s m ()
leftovers xs = PlayerT $ \cs -> foldr Leftover (cs ()) xs
{-# INLINE leftovers #-}

-- | Run a 'PlayerT' action without consuming any input.
lookAhead :: (Functor w, Functor m) => PlayerT w s m a -> PlayerT w s m a
lookAhead pl = PlayerT $ \cs -> go cs [] [] (unPlayerT pl Done) where
  go cs l (x:xs) (Partial f) = go cs l xs (f x)
  go cs l [] (Partial f) = Partial $ \x -> go cs (x : l) [] (f x)
  go cs l xs (Leftover x k) = go cs l (x:xs) k
  go cs l _ (Done a) = foldr Leftover (cs a) l
  go cs l xs (Eff m) = Eff $ fmap (go cs l xs) m
  go cs l xs (Cont m) = Cont $ m . fmap (. go cs l xs)
