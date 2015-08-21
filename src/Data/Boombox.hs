module Data.Boombox (decodeTape) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Data.Functor.Identity

decodeTape :: (Monad m, Monoid s)
  => Tape Identity m s
  -> Decoder e s m a
  -> m (Tape Identity m s, Either e a)
decodeTape (Yield xs (Identity k)) (Partial f) = decodeTape k (f (mconcat xs))
decodeTape (Effect m) d = m >>= \t -> decodeTape t d
decodeTape t (Done s a) = return (Yield [s] (pure t), Right a)
decodeTape t (Failed e) = return (t, Left e)
decodeTape t (Eff m) = m >>= decodeTape t
