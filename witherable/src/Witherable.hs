{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Witherable
-- Copyright   :  (c) Fumiaki Kinoshita 2020
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Witherable
  ( Filterable(..)
  , (<$?>)
  , (<&?>)
  , Witherable(..)
  , ordNub
  , hashNub
  , forMaybe
  -- * Indexed variants
  , FilterableWithIndex(..)
  , WitherableWithIndex(..)
  -- * Wrapper
  , WrappedFoldable(..)
  )

where

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
import Data.Functor.WithIndex.Instances ()
import Data.Witherable.Class
import qualified Data.Set as Set
import qualified Data.HashSet as HSet
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Hashable
import Control.Monad.Trans.State.Strict
import Data.Orphans ()
import Prelude hiding (filter)

-- | An infix alias for 'mapMaybe'. The name of the operator alludes
-- to '<$>', and has the same fixity.
--
-- @since 0.3.1
(<$?>) :: Filterable f => (a -> Maybe b) -> f a -> f b
(<$?>) = mapMaybe
infixl 4 <$?>

-- | Flipped version of '<$?>', the 'Filterable' version of
-- 'Data.Functor.<&>'. It has the same fixity as 'Data.Functor.<&>'.
--
-- @
-- ('<&?>') = 'flip' 'mapMaybe'
-- @
--
-- @since 0.3.1
(<&?>) :: Filterable f => f a -> (a -> Maybe b) -> f b
as <&?> f = mapMaybe f as
infixl 1 <&?>

-- | @'forMaybe' = 'flip' 'wither'@
forMaybe :: (Witherable t, Applicative f) => t a -> (a -> f (Maybe b)) -> f (t b)
forMaybe = flip wither
{-# INLINE forMaybe #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is asymptotically faster than using
--   'Data.List.nub' from "Data.List".
ordNub :: (Witherable t, Ord a) => t a -> t a
ordNub t = evalState (witherM f t) Set.empty where
    f a = state $ \s -> if Set.member a s
      then (Nothing, s)
      else (Just a, Set.insert a s)
{-# INLINE ordNub #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparison (like 'String').
hashNub :: (Witherable t, Eq a, Hashable a) => t a -> t a
hashNub t = evalState (witherM f t) HSet.empty
  where
    f a = state $ \s -> if HSet.member a s
      then (Nothing, s)
      else (Just a, HSet.insert a s)
{-# INLINE hashNub #-}

-- | A default implementation for 'mapMaybe'.
mapMaybeDefault :: (F.Foldable f, Alternative f) => (a -> Maybe b) -> f a -> f b
mapMaybeDefault p = F.foldr (\x xs -> case p x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE mapMaybeDefault #-}

-- | A default implementation for 'imapMaybe'.
imapMaybeDefault :: (FoldableWithIndex i f, Alternative f) => (i -> a -> Maybe b) -> f a -> f b
imapMaybeDefault p = ifoldr (\i x xs -> case p i x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE imapMaybeDefault #-}

newtype WrappedFoldable f a = WrapFilterable {unwrapFoldable :: f a}
  deriving (Functor, F.Foldable, T.Traversable, Applicative, Alternative)

instance (FunctorWithIndex i f) => FunctorWithIndex i (WrappedFoldable f) where
  imap f = WrapFilterable . imap f . unwrapFoldable

instance (FoldableWithIndex i f) => FoldableWithIndex i (WrappedFoldable f) where
  ifoldMap f = ifoldMap f . unwrapFoldable

instance (TraversableWithIndex i f) => TraversableWithIndex i (WrappedFoldable f) where
  itraverse f = fmap WrapFilterable . itraverse f . unwrapFoldable

instance (F.Foldable f, Alternative f) => Filterable (WrappedFoldable f) where
    {-#INLINE mapMaybe#-}
    mapMaybe = mapMaybeDefault

instance (FunctorWithIndex i f, FoldableWithIndex i f, Alternative f) => FilterableWithIndex i (WrappedFoldable f) where
  {-# INLINE imapMaybe #-}
  imapMaybe = imapMaybeDefault

instance (Alternative f, T.Traversable f) => Witherable (WrappedFoldable f)
