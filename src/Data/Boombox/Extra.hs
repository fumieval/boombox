module Data.Boombox.Extra where
import Data.Boombox.Player
import Prelude hiding (takeWhile, dropWhile)

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
