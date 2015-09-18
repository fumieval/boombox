{-# LANGUAGE RankNTypes #-}
module Data.Boombox (
  -- * Constructing boomboxes
   Recorder
  , supplyRecorder
  -- * Composition
  , (@-$), (@->)
  , (>-$), (>->)
  , module Data.Boombox.Tape
  , module Data.Boombox.Player) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Control.Comonad
import Control.Monad
import Control.Monad.Trans.Class
import Data.Void
import Control.Monad.Co

infix 6 @-$
infixl 7 @->
infixr 7 >-$
infixl 8 >->

(@-$) :: (Comonad w, Monad m)
  => Tape w m s
  -> PlayerT w e s m a
  -> m ([s], Tape w m s, Either e a)
t0 @-$ p = go t0 (runPlayerT p) where
  t `go` Done s a = return (s, t, Right a)
  t `go` Failed s e = return (s, t, Left e)
  Yield a wcont `go` Eff m = runCoT m $ extend (\cont -> (Yield a cont `go`)) wcont
  Yield a wcont `go` Partial f = extract wcont `go` f a
  Effect m `go` d = m >>= (`go` d)
{-# INLINE (@-$) #-}

type Recorder e v w m a = Tape w (PlayerT v e a m)

supplyRecorder :: Functor w => [a] -> Recorder e v w m a b -> Recorder e v w m a b
supplyRecorder s = commitTape (leftover s>>)

composeWith :: (Comonad v, Functor w, Monad m, Functor n)
  => (forall x. m x -> n x)
  -> ([a] -> e -> Tape w n b)
  -> Tape v n a
  -> Recorder e v w m a b
  -> Tape w n b
composeWith t h = prepare where
  prepare (Yield a vk) r = downstream [a] vk r
  prepare (Effect m) r = Effect $ fmap (`prepare`r) m

  downstream s vk (Effect d) = go vk $ runPlayerT $ leftover s >> d
  downstream s vk (Yield b w) = Yield b $ fmap (downstream s vk) w

  go vk (Partial f) = extract vk `upstream` f
  go vk (Eff m) = Effect $ t $ runCoT m $ extend (\k -> return . go k) vk
  go vk (Done s r) = downstream s vk r
  go _ (Failed s e) = h s e

  upstream (Effect m) d = Effect $ fmap (`upstream`d) m
  upstream (Yield a vk) d = go vk (d a)
{-# INLINE composeWith #-}

-- | Combine a tape with a recorder. The result will be synchronized with the recorder.
(@->) :: (Comonad v, Functor w, Monad m) => Tape v m a -> Recorder Void v w m a b -> Tape w m b
(@->) = composeWith id (const absurd)
{-# INLINE (@->) #-}

-- | Connect two recorders.
(>->) :: (Comonad u, Comonad v, Functor w, Monad m)
  => Recorder e u v m a b
  -> Recorder e v w m b c
  -> Recorder e u w m a c
(>->) = composeWith lift (const $ Effect . failed)
{-# INLINE (>->) #-}

-- | Combine a recorder with a drive.
(>-$) :: (Comonad v, Comonad w, Monad m) => Recorder e v w m a b -> PlayerT w e b m r -> PlayerT v e a m r
t >-$ p = t `loop` runPlayerT p where
  _ `loop` Done _ r = return r
  _ `loop` Failed _ e = failed e
  Yield b wcont `loop` Partial f = extract wcont `loop` f b
  Yield b wcont `loop` Eff m = join $ lift $ runCoT m $ extend (\w -> return . loop (Yield b w)) wcont
  Effect u `loop` d = go (runPlayerT u) where
    go (Failed s e) = leftover s >> failed e
    go (Partial f) = await >>= go . f
    go (Eff m) = join $ control $ fmap go m
    go (Done s k) = supplyRecorder s k `loop` d
{-# INLINE (>-$) #-}
