{-# LANGUAGE Rank2Types #-}
module Data.Boombox.Boombox where

import Control.Comonad
import Data.Boombox.Tape
import Data.Boombox.Player
import Control.Monad.Trans.Class

infix 6 @.$
infix 6 @-$
infixl 7 @->
infixr 7 >-$
infixl 8 >->

(@.$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w s m a
  -> m a
t0 @.$ p = connectDrive id (\_ _ -> return) [] t0 (runPlayerT p)
{-# INLINE (@.$) #-}

(@-$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w s m a
  -> m ([s], Tape w m s, a)
t0 @-$ p = connectDrive id (\a b c -> return (a, b, c)) [] t0 (runPlayerT p)
{-# INLINE (@-$) #-}

type Boombox v w m a = Tape w (PlayerT v a m)

-- | Combine a tape with a boombox. The result will be synchronized with the boombox.
(@->) :: (Comonad v, Functor w, Monad m) => Tape v m a -> Boombox v w m a b -> Tape w m b
(@->) = composeWith id
{-# INLINE (@->) #-}

-- | Connect two boomboxes.
(>->) :: (Comonad u, Comonad v, Functor w, Monad m)
  => Boombox u v m a b
  -> Boombox v w m b c
  -> Boombox u w m a c
(>->) = composeWith lift
{-# INLINE (>->) #-}

(>-$) :: (Comonad w, Monad m) => Boombox v w m a b -> PlayerT w b m r -> PlayerT v a m r
t0 >-$ p0 = connectDrive lift (\_ _ -> return) [] t0 (runPlayerT p0)
{-# INLINE (>-$) #-}

composeWith :: (Comonad v, Functor w, Monad m, Functor n)
  => (forall x. n x -> m x)
  -> Tape v m a
  -> Boombox v w n a b
  -> Tape w m b
composeWith trans = loop [] where
  loop lo t (Tape m) = Tape $ connectDrive trans
    (\lo' t' (a, w) -> return (a, loop lo' t' <$> w)) lo t (runPlayerT m)
{-# INLINE composeWith #-}

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
      m $ extend (loop lo . yield a) w
{-# INLINE connectDrive #-}
