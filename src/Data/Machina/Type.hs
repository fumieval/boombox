{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveFunctor #-}
module Data.Machina.Type (Machina(..)
  , echo
  , (>->)
  , foldMapping
  , Chronological(..)
  , EventOrder(..)
  , Genesis(..)) where

import Control.Category
import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Identity
import Data.Profunctor
import Control.Comonad
import Prelude hiding (id, (.))
import Data.Functor.Adjunction
import Data.Foldable (toList, traverse_)

-- | 'Machina w m a b' represents a general stream transducer which can:
--
-- * Consume a stream of input values @a@, making a future 'Machina' on the world @m@.
-- * Produce a stream of output values @b@, providing option for next 'Machina' represented by the co-world @w@.
data Machina w m a b = Yield [b] (w (Machina w m a b))
  | Await ([a] -> m (Machina w m a b))
  deriving (Functor)

-- | Same as 'id'.
echo :: (Genesis w, Applicative m) => Machina w m a a
echo = creation $ \k -> Await $ \a -> pure $ Yield a k

(>->) :: (Functor w, Functor m, Adjunction f g) => Machina g m a b -> Machina w f b c -> Machina w m a c
Yield a w >-> Await f = rightAdjunct (\m -> fmap (>->m) w) (f a)
Await f >-> k = Await (fmap (>->k) . f)
k >-> Yield a w = Yield a (fmap (k>->) w)

foldMapping :: (Genesis w, Foldable f, Applicative m) => (a -> f b) -> Machina w m a b
foldMapping f = creation $ \k -> Await $ \a -> pure $ Yield (a >>= toList . f) k

instance (Functor w, Functor m) => Profunctor (Machina w m) where
  dimap f g = go where
    go (Await k) = Await (fmap go . k . fmap f)
    go (Yield b w) = Yield (fmap g b) (fmap go w)

-- | 'Chronological' functor is like 'Apply', but the operation may fail due to a time lag.
class Functor f => Chronological f where
  coincidence :: f a -> f b -> EventOrder (f (a, b))
  
data EventOrder a = Simultaneous a
  | LeftFirst
  | RightFirst
  deriving Functor

instance Chronological Identity where
  coincidence (Identity a) (Identity b) = Simultaneous (Identity (a, b))

instance Ord i => Chronological ((,) i) where
  coincidence (i, a) (j, b) = case compare i j of
    EQ -> Simultaneous (i, (a, b))
    LT -> LeftFirst
    GT -> RightFirst

instance (Chronological w, Functor m) => Apply (Machina w m a) where
  Yield f0 s0 <.> Yield a0 t0 = Yield (f0 <*> a0) $ case coincidence s0 t0 of
    Simultaneous u -> fmap (uncurry (<.>)) u
    LeftFirst -> fmap (bleep a0 t0) s0
    RightFirst -> fmap (bloop f0 s0) t0
    where
      bleep a t = go where
        go (Yield f' s') = Yield (f' <*> a) $ case coincidence s' t of
          Simultaneous u -> fmap (uncurry (<.>)) u
          LeftFirst -> fmap go s'
          RightFirst -> fmap (bloop f' s') t
        go (Await m) = Await (fmap go . m)
      bloop f s = go where
        go (Yield a' t') = Yield (f <*> a') $ case coincidence s t' of
          Simultaneous u -> fmap (uncurry (<.>)) u
          LeftFirst -> fmap (bleep a' t') s
          RightFirst -> fmap go t'
        go (Await m) = Await (fmap go . m)

  Await m <.> t = Await (fmap (<.>t) . m)
  s <.> Await m = Await (fmap (s<.>) . m)

instance (Chronological w, Functor m) => Alt (Machina w m a) where
  Yield a v <!> Yield b w = case coincidence v w of
    Simultaneous u -> Yield (a ++ b) $ fmap (uncurry (<!>)) u
    LeftFirst -> Yield a $ fmap (<!> Yield b w) v
    RightFirst -> Yield b $ fmap (Yield a v <!>) w
  Await m <!> n = Await (fmap (<!>n) . m)
  m <!> Await n = Await (fmap (m<!>) . n)

-- | The class of functors which have their own time series.
class Chronological f => Genesis f where
  creation :: (f r -> r) -> r

instance Genesis Identity where
  creation f = f (Identity (creation f))

instance (Genesis w, Applicative m) => Applicative (Machina w m a) where
  pure a = creation $ Yield [a]
  (<*>) = (<.>)

instance (Genesis w, Applicative m) => Alternative (Machina w m a) where
  empty = creation $ Yield []
  (<|>) = (<!>)
