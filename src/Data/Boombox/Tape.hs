{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Boombox.Tape (Tape(..)
  -- * Consuming tapes
  , headTape
  , cueTape
  -- * Constructing tapes
  , yield
  , yieldMany
  , effect
  , repeater
  -- * Transforming tapes
  , flattenTape
  , filterTape
  , foldTape
  , hoistTransTape
  , hoistTape
  , transTape
  , controlTape
  , pushBack
  , intercept
  -- * Time series
  , Chronological(..)
  , EventOrder(..)
  ) where

import Control.Category
import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Identity
import Prelude hiding (id, (.))
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Comonad.Traced hiding ((<>))
import Data.Semigroup
import Control.Arrow

-- | @Tape w m a@ is a producer of values with a type @a@.
-- It may cause effects @m@ and has a comonadic control @w@.
newtype Tape w m a = Tape { unconsTape :: m (a, w (Tape w m a)) }
  deriving (Functor)

yield :: Applicative m => a -> w (Tape w m a) -> Tape w m a
yield a w = Tape $ pure (a, w)
{-# INLINE yield #-}

effect :: Monad m => m (Tape w m a) -> Tape w m a
effect m = Tape $ m >>= unconsTape
{-# INLINE effect #-}

-- | Build a tape that yields the same value, with the very same effect and exactly the same control.
repeater :: (Functor m, Comonad w) => m (w a) -> Tape w m a
repeater m = Tape $ fmap (\w -> (extract w, repeater m <$ w)) m

-- | Take the first element of the tape.
headTape :: Functor m => Tape w m a -> m a
headTape = fmap fst . unconsTape

-- | Denudate the control without dropping a value.
cueTape :: (Comonad w, Applicative m) => Tape w m a -> m (w (Tape w m a))
cueTape = fmap (\(a, w) -> extend (yield a) w) . unconsTape

-- | Flatten a tape of 'Foldable' containers.
flattenTape :: (Comonad w, Foldable f, Monad m) => Tape w m (f a) -> Tape w m a
flattenTape = foldTape id
{-# INLINE flattenTape #-}

foldTape :: (Comonad w, Foldable f, Monad m) => (a -> f b) -> Tape w m a -> Tape w m b
foldTape f = go where
  go t = Tape $ unconsTape t >>= \(a, w) -> unconsTape $ yieldMany (f a) (fmap go w)
{-# INLINE foldTape #-}

filterTape :: (Comonad w, Monad m) => (a -> Bool) -> Tape w m a -> Tape w m a
filterTape p = go where
  go t = Tape $ unconsTape t >>= \(a, w) -> if p a then return (a, fmap go w) else unconsTape (go (extract w))

yieldMany :: (Comonad w, Foldable f, Applicative m) => f a -> w (Tape w m a) -> Tape w m a
yieldMany f w = extract $ foldr (extend . yield) w f
{-# INLINE yieldMany #-}

-- | Apply a monadic function to a tape.
intercept :: (Functor w, Monad m) => (a -> m b) -> Tape w m a -> Tape w m b
intercept k t = Tape $ unconsTape t >>= \(a, w) -> (\b -> (b, fmap (intercept k) w)) <$> k a

hoistTransTape :: (Functor w, Functor n) => (forall x. v x -> w x) -> (forall x. m x -> n x) -> Tape v m a -> Tape w n a
hoistTransTape s t = go where
  go (Tape m) = Tape $ fmap (\(a, w) -> (a, fmap go (s w))) (t m)
{-# INLINE hoistTransTape #-}

-- | Apply natural transformation to the comonadic control surface.
hoistTape :: (Functor w, Functor m) => (forall x. v x -> w x) -> Tape v m a -> Tape w m a
hoistTape t = hoistTransTape t id
{-# INLINE hoistTape #-}

-- | Transform effects produced by the tape.
transTape :: (Functor w, Functor n) => (forall x. m x -> n x) -> Tape w m a -> Tape w n a
transTape = hoistTransTape id
{-# INLINE transTape #-}

-- | Operate on the control surface just once.
controlTape :: Functor m => (w (Tape w m a) -> w (Tape w m a)) -> Tape w m a -> Tape w m a
controlTape t (Tape m) = Tape $ fmap (second t) m

-- | Push some values back to a tape.
pushBack :: (Foldable f, Comonad w, Monad m) => f a -> Tape w m a -> Tape w m a
pushBack f t = effect $ yieldMany f <$> cueTape t

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

instance (Chronological w, Monad m, Semigroup a)
  => Semigroup (Tape w m a) where
    s <> t = Tape $ do
      (a, v) <- unconsTape s
      (b, w) <- unconsTape t
      case coincidence v w of
        Simultaneous u -> return (a <> b, fmap (uncurry (<>)) u)
        LeftFirst -> return (a, fmap (<> t) v)
        RightFirst -> return (b, fmap (s <>) w)
