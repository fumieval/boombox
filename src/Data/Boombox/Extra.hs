{-# LANGUAGE LambdaCase #-}
module Data.Boombox.Extra where
import Data.Boombox
import Prelude hiding (takeWhile, dropWhile, lines, foldl)

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

foldM :: Monad m => (r -> s -> PlayerT w (Maybe s) m r) -> r -> PlayerT w (Maybe s) m r
foldM f r = await >>= \case
  Just s -> f r s >>= foldM f
  Nothing -> return r

foldl :: Monad m => (r -> s -> r) -> r -> PlayerT w (Maybe s) m r
foldl f r = await >>= \case
  Just s -> foldl f $! f r s
  Nothing -> return r

traverse_ :: Monad m => (s -> PlayerT w (Maybe s) m r) -> PlayerT w (Maybe s) m ()
traverse_ k = await >>= \case
  Just s -> k s >> traverse_ k
  Nothing -> return ()
