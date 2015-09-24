{-# LANGUAGE DeriveFunctor #-}
module Data.Boombox.Head where
import Data.Boombox.Player
import Data.Boombox.Tape
import Control.Comonad
import Control.Applicative

data Head i a = Head !i (Maybe i -> a) deriving Functor

instance Comonad (Head i) where
  extract (Head _ f) = f Nothing
  extend k (Head i f) = Head i $ \m -> k $ Head (maybe i id m) f

instance Ord i => Chronological (Head i) where
  coincidence (Head i f) (Head j g) = case compare i j of
    EQ -> Simultaneous (Head i (liftA2 (,) f g))
    LT -> LeftFirst
    GT -> RightFirst

-- | Seek to an arbitrary position.
seeksTape :: Functor m => (i -> Maybe i) -> Tape (Head i) m a -> Tape (Head i) m a
seeksTape t (Yield _ (Head i f)) = f (t i)
seeksTape t (Effect f) = Effect (fmap (seeksTape t) f)

seeksP :: (i -> Maybe i) -> PlayerT (Head i) s m ()
seeksP t = control $ \(Head i f) -> (f (t i), ())
