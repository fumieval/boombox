module Data.Boombox (
    driveTape
  , (@-$)
  , recording
  , (@->)
  , (-@>)
  , (>-$)
  , (>->)
  , module Data.Boombox.Tape
  , module Data.Boombox.Drive) where
import Data.Boombox.Tape
import Data.Boombox.Drive
import Control.Comonad
import Control.Monad.Trans.Class
import Data.Void

driveTape :: (Comonad w, Monad m)
  => Tape w m s
  -> Drive e s m a
  -> m (Tape w m s, [s], Either e a)
driveTape t (Done s a) = return (t, s, Right a)
driveTape t (Failed s e) = return (t, s, Left e)
driveTape t (Eff m) = m >>= driveTape t
driveTape (Effect m) d = m >>= (`driveTape` d)
driveTape (Yield a wcont) (Partial f) = driveTape (extract wcont) (f a)

-- | Combine a tape with a drive.
(@-$):: (Comonad w, Monad m)
  => Tape w m s
  -> Drive Void s m a
  -> m a
_ @-$ Done _ a = return a
_ @-$ Failed _ e = absurd e
t @-$ Eff m = m >>= (t @-$)
Effect m @-$ d = m >>= (@-$ d)
Yield a wcont @-$ Partial f = extract wcont @-$ f a

recording :: PlayerT e a m (Tape w (Drive e a m) b) -> Tape w (Drive e a m) b
recording = Effect . runPlayerT

-- | Combine a tape with a recorder. The result will be synchronized with the recorder.
(-@>) :: (Comonad v, Functor w, Functor m) => Tape v m a -> Tape w (Drive Void a m) b -> Tape w m b
y@(Yield a vcont) -@> Effect d = case d of
  Partial f -> extract vcont -@> Effect (f a)
  Done !s k -> y -@> commitTape (supplyDrive s) k
  Eff m -> Effect $ fmap ((y -@>) . Effect) m
  Failed _ v -> absurd v
t -@> Yield b w = Yield b $ fmap (t-@>) w
Effect m -@> t = Effect $ fmap (-@>t) m

-- | Combine a tape with a recorder. Unlike ('-@>'), the result will be synchronized with the original tape.
(@->) :: (Comonad w, Comonad v, Monad m) => Tape w m a -> Tape v (Drive Void a m) b -> Tape w m b
Yield a w @-> rec = Effect $ fmap extract $ go rec where
  go (Yield b cont) = extend (Yield b) <$> go (extract cont)
  go (Effect d) = case d of
    Done !s k -> return $ fmap (@->commitTape (supplyDrive s) k) w
    Partial f -> return $ fmap (@->Effect (f a)) w
    Eff m -> m >>= go . Effect
    Failed _ v -> absurd v
Effect m @-> rec = Effect $ fmap (@->rec) m

-- | Combine a recorder with a drive.
(>-$) :: (Comonad w, Functor m) => Tape w (Drive e a m) b -> Drive e b m r -> Drive e a m r
_ >-$ Done _ r = Done [] r
_ >-$ Failed _ e = Failed [] e
t >-$ Eff m = Eff $ fmap (t>-$) m
Effect u >-$ d = go u where
  go (Failed s e) = Failed s e
  go (Partial f) = Partial (go . f)
  go (Eff m) = Eff $ fmap go m
  go (Done s k) = commitTape (supplyDrive s) k >-$ d
Yield b wcont >-$ Partial f = extract wcont >-$ f b

-- | Combine two recorders.
(>->) :: (Comonad v, Functor w, Functor (t m), Monad m, MonadTrans t) => Tape v (t m) a -> Tape w (Drive Void a m) b -> Tape w (t m) b
y@(Yield a vcont) >-> Effect d = case d of
  Partial f -> extract vcont >-> Effect (f a)
  Done s k -> y >-> commitTape (supplyDrive s) k
  Eff m -> Effect $ lift $ fmap ((y >->) . Effect) m
  Failed _ v -> absurd v
t >-> Yield b w = Yield b $ fmap (t>->) w
Effect m >-> t = Effect $ fmap (>->t) m
