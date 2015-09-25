{-# LANGUAGE BangPatterns, LambdaCase #-}
import Data.Boombox
import qualified Data.ByteString as BS
import Control.Monad.Trans
import Data.Functor.Identity
import qualified Data.Boombox.IO as BIO
import System.IO
import System.Environment
import Control.Comonad
import Prelude hiding (lines)

lines :: Comonad w => Boombox w Identity IO (Maybe BS.ByteString) (Maybe BS.ByteString)
lines = Effect (go []) where
  go ls = await >>= \case
    Just c -> do
      let (l, r) = BS.break (==10) c
      if BS.null r
        then go (l : ls)
        else return $ Yield (Just $ BS.concat $ reverse $ l : ls) $ pure
            $ Effect (leftover [Just (BS.tail r)] >> go [])
    Nothing -> return $ case ls of
      [] -> Yield Nothing $ pure $ Effect $ go []
      xs -> Yield (Just (BS.concat (reverse ls))) $ pure $ Effect $ go []

foldlP' :: (r -> s -> r) -> r -> PlayerT w e (Maybe s) m r
foldlP' f r = await >>= \case
  Just s -> foldlP' f $! f r s
  Nothing -> return r

main = do
    [path] <- getArgs
    h <- openFile path ReadMode
    BIO.hGetContentsN 256 h @-> lines
        @-$ do foldlP' (\n _ -> n + 1) (0 :: Int) >>= lift . print
