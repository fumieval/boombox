module Data.Boombox.Extra where
import Data.Boombox.Player
import Prelude hiding (takeWhile, dropWhile)

-- | @peek ≡ lookAhead await@
peek :: PlayerT w e a m a
peek = PlayerT $ \s _ cs -> case s of
  xxs@(x:xs) -> cs xxs x
  [] -> Partial $ \s' -> cs [s'] s'

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
