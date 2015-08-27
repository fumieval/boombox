{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Boombox.Tape (Tape(..)
  , headTape
  , flattenTape
  , yieldMany
  , hoistTransTape
  , hoistTape
  , transTape
  , commitTape
  , Chronological(..)
  , EventOrder(..)
  , Genesis(..)
  , Stepper(..)
  , Needle(..)
  , Reel
  , seekReel) where

import Control.Category
import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Identity
import Prelude hiding (id, (.))
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Comonad.Traced hiding ((<>))
import Data.Reflection
import Data.Proxy
import Data.Semigroup

data Tape w m a = Yield a (w (Tape w m a))
  | Effect (m (Tape w m a))
  deriving (Functor)

headTape :: Monad m => Tape w m a -> m a
headTape (Yield a _) = return a
headTape (Effect m) = m >>= headTape

flattenTape :: (Comonad w, Foldable f, Monad m) => Tape w m (f a) -> Tape w m a
flattenTape (Yield f w) = yieldMany f (fmap flattenTape w)
flattenTape (Effect m) = Effect (fmap flattenTape m)

yieldMany :: (Comonad w, Foldable f) => f a -> w (Tape w m a) -> Tape w m a
yieldMany f w = extract $ foldr (extend . Yield) w f

hoistTransTape :: (Functor w, Functor n) => (forall x. v x -> w x) -> (forall x. m x -> n x) -> Tape v m a -> Tape w n a
hoistTransTape s t = go where
    go (Yield a w) = Yield a $ fmap go $ s w
    go (Effect m) = Effect $ fmap go $ t m
{-# INLINE hoistTransTape #-}

hoistTape :: (Functor w, Functor m) => (forall x. v x -> w x) -> Tape v m a -> Tape w m a
hoistTape t = hoistTransTape t id
{-# INLINE hoistTape #-}

transTape :: (Functor w, Functor n) => (forall x. m x -> n x) -> Tape w m a -> Tape w n a
transTape = hoistTransTape id
{-# INLINE transTape #-}

commitTape :: Functor w => (m (Tape w m a) -> m (Tape w m a)) -> Tape w m a -> Tape w m a
commitTape t (Effect m) = Effect (t m)
commitTape t (Yield a w) = Yield a (commitTape t <$> w)

-- | 'Chronological' functor is like 'Apply', but the operation may fail due to a time lag.
class Functor f => Chronological f where
  coincidence :: f a -> f b -> EventOrder (f (a, b))

data EventOrder a = Simultaneous a
  | LeftFirst
  | RightFirst
  deriving Functor

instance Chronological Identity where
  coincidence (Identity a) (Identity b) = Simultaneous (Identity (a, b))

instance Chronological ((->) i) where
  coincidence f g = Simultaneous $ liftA2 (,) f g

instance Ord i => Chronological ((,) i) where
  coincidence (i, a) (j, b) = case compare i j of
    EQ -> Simultaneous (i, (a, b))
    LT -> LeftFirst
    GT -> RightFirst

instance (Ord i, Chronological w) => Chronological (EnvT i w) where
  coincidence (EnvT i v) (EnvT j w) = case compare i j of
    EQ -> EnvT i <$> coincidence v w
    LT -> LeftFirst
    GT -> RightFirst

instance (Ord i, Chronological w) => Chronological (StoreT i w) where
  coincidence (StoreT v i) (StoreT w j) = case compare i j of
    EQ -> (\wfg -> StoreT (fmap (uncurry $ liftA2 (,)) wfg) i) <$> coincidence v w
    LT -> LeftFirst
    GT -> RightFirst

instance Chronological w => Chronological (TracedT m w) where
  coincidence (TracedT v) (TracedT w) = fmap (TracedT . fmap (uncurry $ liftA2 (,))) $ coincidence v w

instance (Chronological w, Functor m) => Apply (Tape w m) where
  Yield f0 s0 <.> Yield a0 t0 = Yield (f0 a0) $ case coincidence s0 t0 of
    Simultaneous u -> fmap (uncurry (<.>)) u
    LeftFirst -> fmap (bleep a0 t0) s0
    RightFirst -> fmap (bloop f0 s0) t0
    where
      bleep a t = go where
        go (Yield f' s') = Yield (f' a) $ case coincidence s' t of
          Simultaneous u -> fmap (uncurry (<.>)) u
          LeftFirst -> fmap go s'
          RightFirst -> fmap (bloop f' s') t
        go (Effect m) = Effect (fmap go m)
      bloop f s = go where
        go (Yield a' t') = Yield (f a') $ case coincidence s t' of
          Simultaneous u -> fmap (uncurry (<.>)) u
          LeftFirst -> fmap (bleep a' t') s
          RightFirst -> fmap go t'
        go (Effect m) = Effect (fmap go m)

  Effect m <.> t = Effect (fmap (<.>t) m)
  s <.> Effect m = Effect (fmap (s<.>) m)

instance (Chronological w, Functor m, Semigroup a) => Semigroup (Tape w m a) where
  Yield a v <> Yield b w = case coincidence v w of
    Simultaneous u -> Yield (a <> b) $ fmap (uncurry (<>)) u
    LeftFirst -> Yield a $ fmap (<> Yield b w) v
    RightFirst -> Yield b $ fmap (Yield a v <>) w
  Effect m <> n = Effect (fmap (<>n) m)
  m <> Effect n = Effect (fmap (m<>) n)

instance (Genesis w, Functor m, Monoid a, Semigroup a) => Monoid (Tape w m a) where
  mempty = creation $ Yield mempty
  mappend = (<>)

-- | The class of functors which have their own time series.
class Chronological f => Genesis f where
  creation :: (f r -> r) -> r

instance Genesis Identity where
  creation f = f (Identity (creation f))

instance Genesis ((->) i) where
  creation f = f (const (creation f))

instance Genesis w => Genesis (TracedT m w) where
  creation f = creation $ \w -> f $ TracedT (fmap const w)

newtype Stepper s a = Stepper { getStepper :: a } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord a, Reifies s (a -> a, a)) => Genesis ((,) (Stepper s a)) where
  creation k = go a0 where
    go a = k (Stepper a, go (f a))
    (f, a0) = reflect (Proxy :: Proxy s)

instance (Genesis w, Functor m) => Applicative (Tape w m) where
  pure a = creation $ Yield a
  (<*>) = (<.>)

data Needle i a = Needle !i (Maybe i -> a) deriving Functor

instance Comonad (Needle i) where
  extract (Needle _ f) = f Nothing
  extend k (Needle i f) = Needle i $ \m -> k $ Needle (maybe i id m) f

instance Ord i => Chronological (Needle i) where
  coincidence (Needle i f) (Needle j g) = case compare i j of
    EQ -> Simultaneous (Needle i (liftA2 (,) f g))
    LT -> LeftFirst
    GT -> RightFirst

instance (Ord a, Reifies s (a -> a, a)) => Genesis (Needle (Stepper s a)) where
  creation k = go a0 where
    go a = k $ Needle (Stepper a) (maybe (go (f a)) (go . getStepper))
    (f, a0) = reflect (Proxy :: Proxy s)

type Reel i m = Tape (Needle i) m

-- | Seek to an arbitrary position.
seekReel :: Functor m => (i -> Maybe i) -> Reel i m a -> Reel i m a
seekReel t (Yield _ (Needle i f)) = f (t i)
seekReel t (Effect f) = Effect (fmap (seekReel t) f)
