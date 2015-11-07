{-# LANGUAGE LambdaCase #-}
module Data.Boombox.Extra where
import Data.Boombox
import Prelude hiding (takeWhile, dropWhile, lines)
import qualified Data.ByteString as BS
import Control.Comonad
import Data.Functor.Identity

-- | @peek ≡ lookAhead await@
peek :: PlayerT w a m a
peek = await >>= \a -> a <$ leftover a

takeWhile :: (a -> Bool) -> PlayerT w a m [a]
takeWhile p = do
  a <- await
  if p a
    then (a:) <$> takeWhile p
    else leftover a >> return []

dropWhile :: (a -> Bool) -> PlayerT w a m ()
dropWhile p = do
  a <- await
  if p a
    then dropWhile p
    else leftover a

lines :: Comonad w => Boombox w Identity IO (Maybe BS.ByteString) (Maybe BS.ByteString)
lines = Tape (go []) where
  go ls = await >>= \case
    Just c -> do
      let (l, r) = BS.break (==10) c
      if BS.null r
        then go (l : ls)
        else return (Just $ BS.concat $ reverse $ l : ls, pure
            $ Tape $ leftover (Just (BS.tail r)) >> go [])
    Nothing -> return $ case ls of
      [] -> (Nothing, pure $ Tape $ go [])
      _ -> (Just (BS.concat (reverse ls)), pure $ Tape $ go [])
