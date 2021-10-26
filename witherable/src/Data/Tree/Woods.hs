{-# LANGUAGE DeriveFunctor #-}
module Data.Tree.Woods where

import Data.Tree (Tree (..), Forest)
import Data.Maybe (mapMaybe)

newtype MaybeTree a = MaybeTree (Maybe (Tree a))
  deriving (Eq, Show, Functor)

newtype Woods a = Woods { unWoods :: Forest a }
  deriving (Eq, Show, Functor)

forestMapMaybe :: (a -> Maybe b) -> Forest a -> Forest b
forestMapMaybe f = go where
    go xs = mapMaybe g xs

    g (Node y ys) = case f y of
        Just z  -> Just (Node z (go ys))
        Nothing -> Nothing
    
woodsMapMaybe :: (a -> Maybe b) -> Woods a -> Woods b
woodsMapMaybe f (Woods ws) = Woods (forestMapMaybe f ws)
