{-# LANGUAGE DeriveTraversable, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Data.Machina.Seek (
  Needle(..)
  , Transcriber
  , seeks
  , Vinyl
  , Stepper(..)
  ) where
import Data.Machina.Type
import Data.Reflection
import Control.Comonad
import Data.Proxy
import Control.Applicative

data Needle i a = Needle !i (i -> a) a deriving Functor

instance Comonad (Needle i) where
  extract (Needle _ _ a) = a
  duplicate s@(Needle i f a) = Needle i (\i' -> Needle i' f a) s

instance Ord i => Chronological (Needle i) where
  coincidence (Needle i f a) (Needle j g b) = case compare i j of
    EQ -> Simultaneous (Needle i (liftA2 (,) f g) (a, b))
    LT -> LeftFirst
    GT -> RightFirst

type Transcriber i = Machina (Needle i)

type Vinyl i m = Transcriber i m ()

-- | Seek to an arbitrary position.
seeks :: Monad m => (i -> Maybe i) -> Transcriber i m a b -> Transcriber i m a b
seeks t (Yield _ (Needle i f k)) = maybe k f (t i)
seeks t (Await f) = Await (seeks t . f)
seeks t (Past (Needle i f k)) = maybe k f (t i)
seeks t (Future m) = Future (fmap (seeks t) m)

newtype Stepper s a = Stepper { getStepper :: a } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord a, Reifies s (a -> a, a)) => Genesis ((,) (Stepper s a)) where
  creation k = go a0 where
    go a = k (Stepper a, go (f a))
    (f, a0) = reflect (Proxy :: Proxy s)

instance (Ord a, Reifies s (a -> a, a)) => Genesis (Needle (Stepper s a)) where
  creation k = go a0 where
    go a = k $ Needle (Stepper a) (go . getStepper) (go (f a))
    (f, a0) = reflect (Proxy :: Proxy s)
