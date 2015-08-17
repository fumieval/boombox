{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveFunctor #-}
module Data.Machina.Type (Machina(..)
  , echo
  , (>->)
  , foldMapping
  , followFuture
  , fastforward
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

-- | 'Machina w m a b' represents a general stream transducer which can:
--
-- * Consume a stream of input values @a@, making a future 'Machina' on the world @m@.
-- * Produce a stream of output values @b@, providing option for next 'Machina' represented by the co-world @w@.
-- * Stay at the present.
data Machina w m a b = Yield b (w (Machina w m a b))
  | Await (a -> Machina w m a b)
  | Past (w (Machina w m a b))
  | Future (m (Machina w m a b))
  deriving (Functor)

-- | Same as 'id'.
echo :: (Genesis w) => Machina w m a a
echo = creation $ \k -> Await $ \a -> Yield a k

-- | Connect 'Machina' with another which is in the same cosmos.
(>->) :: (Functor w, Functor m) => Machina w m a b -> Machina w m b c -> Machina w m a c
Yield a w >-> Await f = Past $ fmap (>-> f a) w
Await f >-> k = Await ((>->k) . f)
k >-> Yield a w = Yield a (fmap (k>->) w)
Past m >-> n = Past (fmap (>->n) m)
m >-> Past n = Past (fmap (m>->) n)
Future m >-> n = Future (fmap (>->n) m)
m >-> Future n = Future (fmap (m>->) n)

foldMapping :: (Genesis w, Comonad w, Foldable f) => (a -> f b) -> Machina w m a b
foldMapping f = creation $ \k -> Await $ extract . foldr (extend . Yield) k . f

followFuture :: Monad m => (b -> w (Machina w m () b) -> m (Machina w m () b)) -> Machina w m () b -> m (w (Machina w m () b))
followFuture k = go where
  go (Yield b w) = k b w >>= go
  go (Await f) = go (f ())
  go (Future m) = m >>= go
  go (Past w) = return w

-- | @fastforward f = followFuture (\b w -> extract w <$ f b)@
fastforward :: (Comonad w, Monad m) => (b -> m x) -> Machina w m () b -> m (w (Machina w m () b))
fastforward k = go where
  go (Yield b w) = k b >> go (extract w)
  go (Await f) = go (f ())
  go (Future m) = m >>= go
  go (Past w) = return w

instance (Genesis w, Applicative m) => Category (Machina w m) where
  id = echo
  (.) = flip (>->)

instance (Functor w, Functor m) => Profunctor (Machina w m) where
  dimap f g = go where
    go (Await k) = Await (go . k . f)
    go (Yield b w) = Yield (g b) (fmap go w)
    go (Past w) = Past $ fmap go w
    go (Future m) = Future $ fmap go m

-- | 'Chronological' functor is like 'Apply', but the operation may fail due to a time lag.
class Comonad f => Chronological f where
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
        go (Await m) = Await (go . m)
        go (Past w) = Past (fmap go w)
        go (Future m) = Future (fmap go m)
      bloop f s = go where
        go (Yield a' t') = Yield (f a') $ case coincidence s t' of
          Simultaneous u -> fmap (uncurry (<.>)) u
          LeftFirst -> fmap (bleep a' t') s
          RightFirst -> fmap go t'
        go (Await m) = Await (go . m)
        go (Past w) = Past (fmap go w)
        go (Future m) = Future (fmap go m)

  Await m <.> t = Await ((<.>t) . m)
  s <.> Await m = Await ((s<.>) . m)
  Past w <.> t = Past (fmap (<.>t) w)
  s <.> Past w = Past (fmap (s<.>) w)
  Future m <.> t = Future (fmap (<.>t) m)
  s <.> Future m = Future (fmap (s<.>) m)

instance (Chronological w, Functor m) => Alt (Machina w m a) where
  Yield a v <!> Yield b w = case coincidence v w of
    Simultaneous u -> Yield a $ extend (Yield b) $ fmap (uncurry (<!>)) u
    LeftFirst -> Yield a $ fmap (<!> Yield b w) v
    RightFirst -> Yield b $ fmap (Yield a v <!>) w
  Yield a v <!> Past w = case coincidence v w of
    Simultaneous u -> Yield a (fmap (uncurry (<!>)) u)
    LeftFirst -> Yield a (fmap (<!> Past w) v)
    RightFirst -> Past $ fmap (Yield a v <!>) w
  Past v <!> Yield a w = case coincidence v w of
    Simultaneous u -> Yield a (fmap (uncurry (<!>)) u)
    LeftFirst -> Yield a (fmap (Past v<!>) w)
    RightFirst -> Past $ fmap (<!>Yield a w) v
  Past v <!> Past w = case coincidence v w of
    Simultaneous u -> Past $ fmap (uncurry (<!>)) u
    LeftFirst -> Past $ fmap (<!> Past w) v
    RightFirst -> Past $ fmap (<!> Past v) w
  Await m <!> n = Await ((<!>n) . m)
  m <!> Await n = Await ((m<!>) . n)
  Future m <!> t = Future (fmap (<!>t) m)
  s <!> Future m = Future (fmap (s<!>) m)

-- | The class of functors which have their own time series.
class Chronological f => Genesis f where
  creation :: (f r -> r) -> r

instance Genesis Identity where
  creation f = f (Identity (creation f))

instance (Genesis w, Applicative m) => Applicative (Machina w m a) where
  pure a = creation $ Yield a
  (<*>) = (<.>)

instance (Genesis w, Applicative m) => Alternative (Machina w m a) where
  empty = creation Past
  (<|>) = (<!>)
