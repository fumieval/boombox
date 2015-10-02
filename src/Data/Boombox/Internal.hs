{-# LANGUAGE Rank2Types #-}
module Data.Boombox.Internal where

import Control.Comonad
import Data.Boombox.Tape
import Data.Boombox.Player

connectDrive :: (Comonad w, Monad m)
  => (forall x. n x -> m x)
  -> ([s] -> Tape w m s -> a -> m r)
  -> [s]
  -> Tape w m s
  -> Drive w s n a
  -> m r
connectDrive td cont = loop where
  loop lo t d = case d of
    Done a -> cont lo t a
    Partial f -> case lo of
      [] -> do
        (a, w) <- unconsTape t
        loop [] (extract w) (f a)
      (x:xs) -> loop xs t (f x)
    Leftover s k -> loop (s : lo) t k
    Eff m -> td m >>= loop lo t
    Cont m -> do
      (a, w) <- unconsTape t
      m $ extend (loop lo . Yield a) w
{-# INLINE connectDrive #-}
