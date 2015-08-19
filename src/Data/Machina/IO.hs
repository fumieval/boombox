module Data.Machina.IO where

import Data.Machina.Type
import Data.Machina.Seek
import Control.Monad.IO.Class
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Data.Int

hGetContentsN :: MonadIO m => Int -> IO.Handle -> Vinyl Int64 m BS.ByteString
hGetContentsN n h = Await $ const $ go 0 where
  go i = do
    c <- liftIO $ BS.hGetSome h n
    let l = BS.length c
    return $ if l <= 0
      then Yield [] $ fmap (Await . const) $ Needle i go (go i)
      else Yield [c] $ Needle i
        (\j -> Await $ const $ liftIO (IO.hSeek h IO.AbsoluteSeek (fromIntegral j)) >> go j)
        (Await $ const $ go (i + fromIntegral l))

hGetContents :: MonadIO m => IO.Handle -> Vinyl Int64 m BS.ByteString
hGetContents = hGetContentsN 4080
