{-# LANGUAGE LambdaCase #-}
module Data.Boombox.Combinators where

foldMFrom :: Monad m => m (Maybe s) -> (r -> s -> m r) -> r -> m r
foldMFrom m f r = m >>= \case
  Just s -> f r s >>= foldMFrom m f
  Nothing -> return r

foldlFrom :: Monad m => m (Maybe s) -> (r -> s -> r) -> r -> m r
foldlFrom m f r = m >>= \case
  Just s -> foldlFrom m f $! f r s
  Nothing -> return r

traverseFrom_ :: Monad m => m (Maybe s) -> (s -> m r) -> m ()
traverseFrom_ m k = m >>= \case
  Just s -> k s >> traverseFrom_ m k
  Nothing -> return ()