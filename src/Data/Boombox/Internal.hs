{-# LANGUAGE Rank2Types #-}
module Data.Boombox.Internal where

import Control.Comonad
import Data.Boombox.Tape
import Data.Boombox.Player

connectDrive :: (Comonad w, Monad m)
  => (forall x. n x -> m x)
  -> [s]
  -> Tape w m s
  -> Drive w s n a
  -> m ([s], Tape w m s, a)
connectDrive td = loop where
  loop lo t (Done a) = return (lo, t, a)
  loop lo t (Eff m) = td m >>= loop lo t
  loop lo (Yield a wcont) (Cont m) = m $ extend (loop lo . Yield a) wcont
  loop [] (Yield a wcont) (Partial f) = loop [] (extract wcont) (f a)
  loop (x:xs) t (Partial f) = loop xs t (f x)
  loop lo (Effect m) d = m >>= loop lo `flip` d
  loop lo t (Leftover s k) = loop (s : lo) t k
{-# INLINE connectDrive #-}
