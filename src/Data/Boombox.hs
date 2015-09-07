{-# LANGUAGE RankNTypes #-}
module Data.Boombox (
    driveTape
  , (@-$)
  , recording
  -- * Simple composition
  , Recorder
  , composeWith
  , (-@>)
  , (>->)
  , (>-$)
  , module Data.Boombox.Tape
  , module Data.Boombox.Drive) where
import Data.Boombox.Tape
import Data.Boombox.Drive
import Control.Comonad
import Control.Monad.Trans.Class
import Data.Void
import Control.Monad.Co

driveTape :: (Comonad w, Monad m)
  => Tape w m s
  -> Drive w e s m a
  -> m (Tape w m s, [s], Either e a)
driveTape t (Done s a) = return (t, s, Right a)
driveTape t (Failed s e) = return (t, s, Left e)
driveTape (Yield a wcont) (Eff m) = runCoT m $ extend (\cont -> driveTape (Yield a cont)) wcont
driveTape (Yield a wcont) (Partial f) = driveTape (extract wcont) (f a)
driveTape (Effect m) d = m >>= (`driveTape` d)

-- | Combine a tape with a drive.
(@-$):: (Comonad w, Monad m)
  => Tape w m s
  -> Drive w Void s m a
  -> m a
t @-$ d = do
  (_, _, Right a) <- driveTape t d
  return a

recording :: PlayerT v e a m (Tape w (Drive v e a m) b) -> Tape w (Drive v e a m) b
recording = Effect . runPlayerT

type Recorder e v w m a = Tape w (Drive v e a m)

composeWith :: (Comonad v, Functor w, Monad m, Functor n)
  => (forall x. m x -> n x)
  -> ([a] -> e -> Tape w n b)
  -> Tape v n a
  -> Recorder e v w m a b
  -> Tape w n b
composeWith t h = loop where
  loop (Yield a vk0) (Effect d0) = go [] vk0 d0 where
    go (x:xs) vk (Partial f) = go xs vk (f x)
    go [] vk (Partial f) = extract vk `loop` Effect (f a)
    go xs vk (Eff m) = Effect $ t $ runCoT m $ extend (\k -> return . go xs k) vk
    go xs vk (Done s r) = finish (s ++ xs) vk r
    go _ _ (Failed s e) = h s e

    finish xs vk (Effect d1) = go xs vk d1
    finish xs vk (Yield b w) = Yield b $ fmap (finish xs vk) w

  loop tp (Yield b w) = Yield b $ fmap (loop tp) w
  loop (Effect m) r = Effect $ fmap (`loop`r) m

-- | Combine a tape with a recorder. The result will be synchronized with the recorder.
(-@>) :: (Comonad v, Functor w, Monad m) => Tape v m a -> Recorder Void v w m a b -> Tape w m b
(-@>) = composeWith id (const absurd)

-- | Connect two recorders.
(>->) :: (Comonad u, Comonad v, Functor w, Monad m)
  => Recorder e u v m a b
  -> Recorder e v w m b c
  -> Recorder e u w m a c
(>->) = composeWith (Eff . lift . fmap return) (const $ Effect . Failed [])

-- | Combine a recorder with a drive.
(>-$) :: (Comonad v, Comonad w, Monad m) => Tape w (Drive v e a m) b -> Drive w e b m r -> Drive v e a m r
_ >-$ Done _ r = Done [] r
_ >-$ Failed _ e = Failed [] e
Yield b wcont >-$ Partial f = extract wcont >-$ f b
Yield b wcont >-$ Eff m = Eff $ lift $ runCoT m $ extend (\w -> return . (Yield b w >-$)) wcont
Effect u >-$ d = go u where
  go (Failed s e) = Failed s e
  go (Partial f) = Partial (go . f)
  go (Eff m) = Eff $ fmap go m
  go (Done s k) = commitTape (supplyDrive s) k >-$ d
