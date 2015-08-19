{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Data.Machina.Seek (
  Needle(..)
  , seeks
  , Tape
  ) where
import Data.Machina.Type
import Data.Reflection
import Control.Comonad
import Control.Applicative
import Data.Proxy

-- | Index, seeker, next
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

type Tape i m = Machina (Needle i) m

-- | Seek to an arbitrary position.
seeks :: Functor m => (i -> Maybe i) -> Tape i m a -> Tape i m a
seeks t (Yield _ (Needle i f)) = f (t i)
seeks t (Effect f) = Effect (fmap (seeks t) f)
