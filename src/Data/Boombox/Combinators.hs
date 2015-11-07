{-# LANGUAGE LambdaCase #-}
module Data.Boombox.Combinators where

foldlFrom :: Monad m => m (Maybe s) -> (r -> s -> r) -> r -> m r
foldlFrom m f r = m >>= \case
  Just s -> foldlFrom m f $! f r s
  Nothing -> return r
