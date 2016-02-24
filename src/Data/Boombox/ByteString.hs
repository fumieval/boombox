{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Data.Boombox.ByteString where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Boombox.Player
import Control.Monad
import Control.Monad.Trans.Class
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
