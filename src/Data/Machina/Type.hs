{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveFunctor #-}
module Data.Machina.Type (Machina(..)
  , echo
  , (>-|>)
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
import Prelude hiding (id, (.))
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.Foldable (toList)
import Control.Comonad.Env
import Control.Comonad.Store
import Control.Comonad.Traced hiding (Alt)

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

connectWith :: (Functor w, Functor m, Functor f, Functor g)
  => (f (g (Machina w m a c)) -> Machina w m a c)
  -> Machina w f b c
  -> Machina g m a b
  -> Machina w m a c
connectWith t = go where
  go (Await f) (Yield b w) = t $ fmap (\m -> fmap (go m) w) (f b)
  go (Yield c w) k = Yield c (fmap (`go`k) w)
  go k (Await f) = Await (fmap (go k) . f)

(>-|>) :: (Functor w, Functor m, Adjunction f g) => Machina g m a b -> Machina w f b c -> Machina w m a c
f >-|> g = connectWith counit g f

(>->) :: (Functor w, Functor m) => Machina w m a b -> Machina w m b c -> Machina w m a c
m >-> Yield c w = Yield c (fmap (m>->) w)
Await f >-> k = Await (fmap (>->k) . f)
Yield b w >-> k = Yield [] (fmap (>->supply b k) w)

supply :: (Functor w, Functor m) => [a] -> Machina w m a b -> Machina w m a b
supply a (Await f) = Await $ \a' -> f (a ++ a') 
supply a (Yield b w) = Yield b (fmap (supply a) w)

foldMapping :: (Genesis w, Foldable f, Applicative m) => (a -> f b) -> Machina w m a b
foldMapping f = creation $ \k -> Await $ \a -> pure $ Yield (a >>= toList . f) k

instance (Genesis w, Applicative m) => Category (Machina w m) where
  id = echo
  (.) = flip (>->)

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

instance Chronological ((->) i) where
  coincidence f g = Simultaneous $ mzipRep f g

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
    EQ -> (\wfg -> StoreT (fmap (uncurry mzipRep) wfg) i) <$> coincidence v w
    LT -> LeftFirst
    GT -> RightFirst

instance Chronological w => Chronological (TracedT m w) where
  coincidence (TracedT v) (TracedT w) = fmap (TracedT . fmap (uncurry mzipRep)) $ coincidence v w

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

instance Genesis ((->) i) where
  creation f = f (const (creation f))

instance Genesis w => Genesis (TracedT m w) where
  creation f = creation $ \w -> f $ TracedT (fmap const w)

instance (Genesis w, Applicative m) => Applicative (Machina w m a) where
  pure a = creation $ Yield [a]
  (<*>) = (<.>)

instance (Genesis w, Applicative m) => Alternative (Machina w m a) where
  empty = creation $ Yield []
  (<|>) = (<!>)
