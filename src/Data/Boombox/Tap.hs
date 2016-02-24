module Data.Boombox.Tap where

import qualified Control.Applicative as A
import Data.Boombox.Tape
import Data.Functor.Identity

empty :: A.Alternative m => Tape w m a
empty = Tape A.empty

singleton :: A.Alternative m => a -> Tape Identity m a
singleton = flip yield (Identity empty)

tap :: (Foldable f, A.Alternative m) => f a -> Tape Identity m a
tap = flip yieldMany (Identity empty)
