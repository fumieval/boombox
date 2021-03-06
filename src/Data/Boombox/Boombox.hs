{-# LANGUAGE Rank2Types, LambdaCase #-}
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

-- | Feed a tape to a player and extract the final result.
-- The remaining 'Tape' and the unconsumed data will be discarded.
(@.$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w s m a
  -> m a
t0 @.$ p = connectDrive id (\_ _ -> return) [] t0 (runPlayerT p)
{-# INLINE (@.$) #-}

-- | Feed a tape to a player. It returns the leftover input, the remainder of the tape, and the result from the player.
(@-$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w s m a
  -> m ([s], Tape w m s, a)
t0 @-$ p = connectDrive id (\a b c -> return (a, b, c)) [] t0 (runPlayerT p)
{-# INLINE (@-$) #-}

-- | @'Boombox' v w m a b@ is a transducer from @a@ to @b@ with monadic effect @m@, a comonadic control @v@ (outgoing) and @w@ (incoming).
type Recorder v w m a = Tape w (PlayerT v a m)

-- | Combine a tape with a boombox. The result will be synchronized with the boombox.
(@->) :: (Comonad v, Functor w, Monad m) => Tape v m a -> Recorder v w m a b -> Tape w m b
(@->) = recordWith id
{-# INLINE (@->) #-}

-- | Connect two boomboxes.
(>->) :: (Comonad v, Functor w, Monad m)
  => Recorder u v m a b
  -> Recorder v w m b c
  -> Recorder u w m a c
(>->) = recordWith lift
{-# INLINE (>->) #-}

-- | Connect a boombox to a player.
(>-$) :: (Comonad w, Monad m) => Recorder v w m a b -> PlayerT w b m r
  -> PlayerT v a m ([b], Recorder v w m a b, r)
t0 >-$ p0 = connectDrive lift (\a b c -> return (a, b, c)) [] t0 (runPlayerT p0)
{-# INLINE (>-$) #-}

recordWith :: (Comonad v, Functor w, Monad m)
  => (forall x. n x -> m x)
  -> Tape v m a
  -> Recorder v w n a b
  -> Tape w m b
recordWith trans = loop [] where
  loop lo t (Tape m) = Tape $ connectDrive trans
    (\lo' t' (a, w) -> return (a, loop lo' t' <$> w)) lo t (runPlayerT m)
{-# INLINE recordWith #-}

connectDrive :: (Comonad w, Monad m)
  => (forall x. n x -> m x)
  -> ([s] -> Tape w m s -> a -> m r) -- leftover, leftover tape, result
  -> [s] -- initial supply
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

recordMaybe :: (Functor v, Comonad w, Monad m) => Recorder v w m a b -> Recorder v w m (Maybe a) (Maybe b)
recordMaybe boom = Tape $ go [] $ runPlayerT $ unconsTape boom where
  go xs (Done (a, wk)) = return (Just a, fmap (Tape . go xs . runPlayerT . unconsTape) wk)
  go (x:xs) (Partial f) = go xs (f x)
  go [] (Partial f) = await >>= \case
      Just a -> go [] (f a)
      Nothing -> do
        (b, w) <- unconsTape (recordMaybe boom)
        return (Nothing, extend (yield b) w)
  go xs (Leftover x k) = go (x:xs) k
  go xs (Eff m) = lift m >>= go xs
  go xs (Cont m) = control $ \w -> m $ fmap (\a d -> (a, go xs d)) w

interactRecorder :: (Functor v, Monad m)
  => v (m ())
  -> m a
  -> (forall x. b -> w x -> m x)
  -> Recorder v w m a b -> m ()
interactRecorder req get put = go [] . runPlayerT . unconsTape where
  go xs (Done (a, wk)) = put a wk >>= go xs . runPlayerT . unconsTape
  go (x:xs) (Partial f) = go xs (f x)
  go [] (Partial f) = get >>= go [] . f
  go xs (Leftover x k) = go (x:xs) k
  go xs (Eff m) = m >>= go xs
  go xs (Cont m) = m $ fmap (\t -> (t >>) . go xs) req
