{-# LANGUAGE DeriveFunctor, LambdaCase #-}
module Data.Boombox.Head where
import Data.Boombox.Player
import Data.Boombox.Tape
import Control.Comonad
import Control.Applicative

-- | 'Head' is a Store-like comonad which handles seeking.
data Head i a = Head !i (Maybe i -> a) deriving Functor

instance Comonad (Head i) where
  extract (Head _ f) = f Nothing
  extend k (Head i f) = Head i $ \case
    Nothing -> k $ Head i f
    Just j -> k $ Head j $ f . Just . maybe j id

instance Ord i => Chronological (Head i) where
  coincidence (Head i f) (Head j g) = case compare i j of
    EQ -> Simultaneous (Head i (liftA2 (,) f g))
    LT -> LeftFirst
    GT -> RightFirst

-- | Seek to an arbitrary position.
seeksTape :: Monad m => (i -> Maybe i) -> Tape (Head i) m a -> Tape (Head i) m a
seeksTape t (Tape m) = Tape $ m >>= \(_, Head i f) -> unconsTape (f (t i))

-- | Get the current offset.
posP :: PlayerT (Head i) s m i
posP = control $ \(Head i f) -> (f Nothing, pure i)

-- | Apply the given function to the current offset and jump to the resulting offset.
seeksP :: (i -> Maybe i) -> PlayerT (Head i) s m ()
seeksP t = control $ \(Head i f) -> (f (t i), pure ())

-- | Seek to the given offset.
seekP :: i -> PlayerT (Head i) s m ()
seekP i = seeksP (const (Just i))
