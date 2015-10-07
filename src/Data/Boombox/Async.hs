module Data.Boombox.Async where

import Control.Concurrent
import Data.Boombox.Tape
import Data.Foldable
import Data.Function

-- \ Merge multiple tapes in an asynchronous manner.
asyncMergeTapes :: Functor w => [Tape w IO a] -> Tape w IO a
asyncMergeTapes ts = Tape $ do
  v <- newChan
  let go k t = do
        (a, w) <- unconsTape t
        writeChan v (a, w, k)
        takeMVar k >>= go k
  for_ ts $ \t -> do
    k <- newEmptyMVar
    forkIO $ go k t
  fix $ \self -> do
    (a, w, k) <- readChan v
    return (a, fmap (\cont -> Tape $ do
      putMVar k $! cont
      self) w)
