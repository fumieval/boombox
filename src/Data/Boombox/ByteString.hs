{-# LANGUAGE Rank2Types, ScopedTypeVariables, LambdaCase #-}
module Data.Boombox.ByteString where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Control.Monad
import Control.Monad.Trans.Class
import Data.Boombox.Player
import Data.Boombox.Boombox
import Data.Boombox.Tape
import Data.Functor.Identity
import System.IO.Unsafe

awaitStorable :: forall w m a. (MonadPlus m, Storable a) => PlayerT w B.ByteString m a
awaitStorable = do
  B.PS fptr ofs len <- await
  let ofs' = ofs + sizeOf (undefined :: a)
  lift $ guard $ ofs' <= len
  leftover $ B.PS fptr ofs' len
  return $! unsafePerformIO $ withForeignPtr fptr $ peek . castPtr

awaitByteString :: Int -> PlayerT w B.ByteString m B.ByteString
awaitByteString n = do
  bs <- await
  let (b, bs') = B.splitAt n bs
  leftover bs'
  return b

lines :: Recorder w Identity IO (Maybe B.ByteString) (Maybe B.ByteString)
lines = Tape (go []) where
  go ls = await >>= \case
    Just c -> do
      let (l, r) = B.break (==10) c
      if B.null r
        then go (l : ls)
        else return (Just $ B.concat $ reverse $ l : ls, pure
            $ Tape $ leftover (Just (B.tail r)) >> go [])
    Nothing -> return $ case ls of
      [] -> (Nothing, pure $ Tape $ go [])
      _ -> (Just (B.concat (reverse ls)), pure $ Tape $ go [])
