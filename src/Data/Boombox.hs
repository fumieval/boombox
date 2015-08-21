module Data.Boombox (($$), Transcoder(..), (>->)) where
import Data.Boombox.Tape
import Data.Boombox.Player
import Control.Comonad

decodeTape :: (Comonad w, Monad m)
  => Tape w m s
  -> Decoder e s m a
  -> m (Tape w m s, [s], Either e a)
decodeTape (Effect m) d = m >>= \t -> go t d
decodeTape (Yield s wcont) d = pour (extract wcont) s d where
  pour t (x:xs) (Partial f) = pour t xs (f x)
  pour t [] p = go t p
  pour t xs (Eff m) = m >>= pour t xs
  pour t xs (Done s a) = return (t, xs ++ s, Right a)
  pour t xs (Failed e) = return (t, xs, Left e)

newtype Transcoder w a b m r = Transcoder { unTranscoder :: Tape w (Decoder r a m) b }

(>->) :: (Comonad w, Functor f, Functor m)
  => Transcoder w a b m r
  -> Transcoder f b c m r
  -> Transcoder f a c m r
Transcoder s0 >-> Transcoder t0 = Transcoder (go s0 [] t0) where
  go (Effect m) xs t = Effect $ upstream t xs m
  go t xs (Yield c w) = Yield c $ fmap (go t xs) w
  go (Yield b wcont) xs (Effect m) = Effect $ downstream (extract wcont) (xs ++ b) m

  upstream cont xs (Partial f) = Partial $ \s -> upstream cont xs (f s)
  upstream cont xs (Done ss t) = Done ss $ go t xs cont
  upstream cont xs (Eff m) = Eff $ fmap (upstream cont xs) m
  upstream _ _ (Failed e) = Failed e

  downstream cont (x:xs) (Partial f) = downstream cont xs (f x)
  downstream cont xs (Eff m) = Eff $ fmap (downstream cont xs) m
  downstream cont xs (Done ss t) = Done [] $ go cont (xs ++ ss) t
  downstream _ _ (Failed e) = Failed e
  downstream (Yield b wcont) [] p = downstream (extract wcont) b p
  downstream (Effect m) [] p = upstream (Effect p) [] m

-- (>-$) :: Transcoder w a b m e -> Decoder e b m r -> Decoder e a m r
-- (@->) :: Tape w m a -> Transcoder w a b m Void -> Tape w m b
