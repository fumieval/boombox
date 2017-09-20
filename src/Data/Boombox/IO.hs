module Data.Boombox.IO where

import Data.Boombox.Tape
import Data.Boombox.Head
import Control.Monad.IO.Class
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Data.Int

hGetContentsN :: MonadIO m => Int -> IO.Handle -> Tape (Head Int64) m (Maybe BS.ByteString)
hGetContentsN n h = go 0 where
  go i = Tape $ do
    c <- liftIO $ BS.hGetSome h n
    let l = BS.length c
    if l <= 0
      then return (Nothing, Head i $ maybe (go i) go)
      else return (Just c, Head i $ maybe
        (go (i + fromIntegral l))
        (\j -> Tape $ liftIO (IO.hSeek h IO.AbsoluteSeek (fromIntegral j)) >> unconsTape (go j)))

hGetContents :: MonadIO m => IO.Handle -> Tape (Head Int64) m (Maybe BS.ByteString)
hGetContents = hGetContentsN 4080

readFile :: MonadIO m => FilePath -> Tape (Head Int64) m (Maybe BS.ByteString)
readFile path = effectTape $ hGetContents <$> liftIO (IO.openFile path IO.ReadMode)
