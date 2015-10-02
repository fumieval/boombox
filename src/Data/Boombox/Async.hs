module Data.Boombox.Async where

import Control.Concurrent
import Data.Boombox.Tape
import Data.Foldable
import Data.Function

-- \ Merge multiple tapes in an asynchronous manner.
asyncMergeTapes :: Functor w => [Tape w IO a] -> Tape w IO a
asyncMergeTapes ts = Effect $ do
  v <- newChan
  let go k (Yield a w) = do
        writeChan v (a, w, k)
        takeMVar k >>= go k
      go k (Effect m) = m >>= go k
  for_ ts $ \t -> do
    k <- newEmptyMVar
    forkIO $ go k t
  fix $ \self -> do
    (a, w, k) <- readChan v
    return $ Yield a $ fmap (\cont -> Effect $ do
      putMVar k $! cont
      self) w
