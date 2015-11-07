{-# LANGUAGE RankNTypes #-}
module Data.Boombox (
   Boombox
  , (@.$)
  , (@-$), (@->)
  , (>-$), (>->)
  , module Data.Boombox.Tape
  , module Data.Boombox.Player
  , module Data.Boombox.Combinators
  , module Data.Boombox.Async) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Data.Boombox.Internal
import Data.Boombox.Async
import Data.Boombox.Combinators
import Control.Comonad
import Control.Monad.Trans.Class

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

composeWith :: (Comonad v, Functor w, Monad m, Functor n)
  => (forall x. n x -> m x)
  -> Tape v m a
  -> Boombox v w n a b
  -> Tape w m b
composeWith trans = loop [] where
  loop lo t (Tape m) = Tape $ connectDrive trans
    (\lo' t' (a, w) -> return (a, fmap (loop lo' t') w)) lo t (runPlayerT m)
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
