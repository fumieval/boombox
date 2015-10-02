{-# LANGUAGE RankNTypes #-}
module Data.Boombox (
   Boombox
  , (@-$), (@->)
  , (>-$), (>->)
  , module Data.Boombox.Tape
  , module Data.Boombox.Player) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Data.Boombox.Internal
import Control.Comonad
import Control.Monad.Trans.Class

infix 6 @-$
infixl 7 @->
infixr 7 >-$
infixl 8 >->

(@-$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w s m a
  -> m ([s], Tape w m s, a)
t0 @-$ p = connectDrive id (\a b c -> return (a, b, c)) [] t0 (runPlayerT p)
{-# INLINE (@-$) #-}

type Boombox v w m a = Tape w (PlayerT v a m)

composeWith :: (Comonad v, Functor w, Monad m, Functor n)
  => (forall x. n x -> m x)
  -> Tape v m a
  -> Boombox v w n a b
  -> Tape w m b
composeWith trans = loop [] where
  loop lo t (Effect m) = Effect $ connectDrive trans (\a b -> return . loop a b) lo t (runPlayerT m)
  loop lo t (Yield b wk) = Yield b $ loop lo t <$> wk
{-# INLINE composeWith #-}

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
