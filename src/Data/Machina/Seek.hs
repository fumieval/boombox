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

type Transcriber i = Machina (Needle i)

type Vinyl i m = Transcriber i m ()

-- | Seek to an arbitrary position.
seeks :: Functor m => (i -> Maybe i) -> Transcriber i m a b -> Transcriber i m a b
seeks t (Yield _ (Needle i f)) = f (t i)
seeks t (Await f) = Await (fmap (seeks t) . f)

newtype Stepper s a = Stepper { getStepper :: a } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord a, Reifies s (a -> a, a)) => Genesis ((,) (Stepper s a)) where
  creation k = go a0 where
    go a = k (Stepper a, go (f a))
    (f, a0) = reflect (Proxy :: Proxy s)

instance (Ord a, Reifies s (a -> a, a)) => Genesis (Needle (Stepper s a)) where
  creation k = go a0 where
    go a = k $ Needle (Stepper a) (maybe (go (f a)) (go . getStepper))
    (f, a0) = reflect (Proxy :: Proxy s)
