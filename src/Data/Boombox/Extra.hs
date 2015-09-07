module Data.Boombox.Extra where
import Data.Boombox.Drive
import Prelude hiding (takeWhile, dropWhile)

peek :: PlayerT w e a m a
peek = do
  a <- await
  leftover [a]
  return a

takeWhile :: (a -> Bool) -> PlayerT w e a m [a]
takeWhile p = do
  a <- await
  if p a
    then (a:) <$> takeWhile p
    else leftover [a] >> return []

dropWhile :: (a -> Bool) -> PlayerT w e a m ()
dropWhile p = do
  a <- await
  if p a
    then dropWhile p
    else leftover [a]
