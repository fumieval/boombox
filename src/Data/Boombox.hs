module Data.Boombox (decodeTape, (@->), (>-$), module Data.Boombox.Tape, module Data.Boombox.Player) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Control.Comonad
import Data.Void

decodeTape :: (Comonad w, Monad m)
  => Tape w m s
  -> Decoder e s m a
  -> m (Tape w m s, [s], Either e a)
decodeTape t (Done s a) = return (t, s, Right a)
decodeTape t (Failed s e) = return (t, s, Left e)
decodeTape t (Eff m) = m >>= decodeTape t
decodeTape (Effect m) d = m >>= (`decodeTape` d)
decodeTape (Yield a wcont) (Partial f) = decodeTape (extract wcont) (f a)

commitTape :: Functor w => (m (Tape w m a) -> m (Tape w m a)) -> Tape w m a -> Tape w m a
commitTape t (Effect m) = Effect (t m)
commitTape t (Yield a w) = Yield a (commitTape t <$> w)

(@->) :: (Comonad v, Functor w, Functor m) => Tape v m a -> Tape w (Decoder' a m) b -> Tape w m b
y@(Yield a vcont) @-> Effect d = case d of
  Partial f -> extract vcont @-> Effect (f a)
  Done s k -> y @-> commitTape (supplyDecoder s) k
  Eff m -> Effect $ fmap ((y @->) . Effect) m
  Failed _ v -> absurd v
t @-> Yield b w = Yield b $ fmap (t @->) w
Effect m @-> t = Effect $ fmap (@->t) m

(>-$) :: (Comonad w, Functor m) => Tape w (Decoder e a m) b -> Decoder e b m r -> Decoder e a m r
_ >-$ Done _ r = Done [] r
_ >-$ Failed _ e = Failed [] e
t >-$ Eff m = Eff $ fmap (t>-$) m
Effect u >-$ d = go u where
  go (Failed s e) = Failed s e
  go (Partial f) = Partial $ go . f
  go (Eff m) = Eff $ fmap go m
  go (Done s k) = commitTape (supplyDecoder s) k >-$ d
Yield b wcont >-$ Partial f = extract wcont >-$ f b
