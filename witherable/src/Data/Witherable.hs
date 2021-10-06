{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Witherable
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Witherable {-# DEPRECATED "Use Witherable instead" #-}
  ( Filterable(..)
  , (<$?>)
  , (<&?>)
  , Witherable(..)
  , ordNub
  , ordNubOn
  , hashNub
  , hashNubOn
  , forMaybe
  -- * Indexed variants
  , FilterableWithIndex(..)
  , WitherableWithIndex(..)
  -- * Generalization
  , WitherLike, Wither, WitherLike', Wither'
  , FilterLike, Filter, FilterLike', Filter'
  , witherOf
  , forMaybeOf
  , mapMaybeOf
  , catMaybesOf
  , filterAOf
  , filterOf
  , ordNubOf
  , ordNubOnOf
  , hashNubOf
  , hashNubOnOf
   -- * Cloning
  , cloneFilter
  , Peat(..)
  -- * Wrapper
  , WrappedFoldable(..)
  ) where

import Control.Applicative
import Data.Functor.Identity
import Witherable
import qualified Data.Set as Set
import qualified Data.HashSet as HSet
import Control.Monad.Trans.State.Strict
import Data.Hashable
import Data.Coerce

type Filter s t a b = Wither s t a b
{-# DEPRECATED Filter "Use Wither instead" #-}
type FilterLike f s t a b = WitherLike f s t a b
{-# DEPRECATED FilterLike "Use WitherLike instead" #-}
type Filter' s a = Wither' s a
{-# DEPRECATED Filter' "Use Filter' instead" #-}
type FilterLike' f s a = WitherLike' f s a
{-# DEPRECATED FilterLike' "Use WitherLike' instead" #-}

-- | This type allows combinators to take a 'Filter' specializing the parameter @f@.
type WitherLike f s t a b = (a -> f (Maybe b)) -> s -> f t

-- | A 'Wither' is like a <http://hackage.haskell.org/package/lens-4.13.2.1/docs/Control-Lens-Type.html#t:Traversal Traversal>,
-- but you can also remove targets.
type Wither s t a b = forall f. Applicative f => WitherLike f s t a b

-- | A simple 'WitherLike'.
type WitherLike' f s a = WitherLike f s s a a

-- | A simple 'Wither'.
type Wither' s a = forall f. Applicative f => WitherLike' f s a

-- | This is used to characterize and clone a 'Filter'.
-- Since @FilterLike (Peat a b) s t a b@ is monomorphic, it can be used to store a filter in a container.
newtype Peat a b t = Peat { runPeat :: forall f. Applicative f => (a -> f (Maybe b)) -> f t }

instance Functor (Peat a b) where
  fmap f (Peat k) = Peat (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (Peat a b) where
  pure a = Peat $ const (pure a)
  {-# INLINE pure #-}
  Peat f <*> Peat g = Peat $ \h -> f h <*> g h
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 f (Peat xs) (Peat ys) = Peat $ \h -> liftA2 f (xs h) (ys h)
  {-# INLINE liftA2 #-}
#endif

-- | Reconstitute a 'Filter' from its monomorphic form.
cloneFilter :: FilterLike (Peat a b) s t a b -> Filter s t a b
cloneFilter l f = (\a -> a `runPeat` f) . l (\a -> Peat $ \g -> g a)
{-# INLINABLE cloneFilter #-}

-- | 'witherOf' is actually 'id', but left for consistency.
witherOf :: FilterLike f s t a b -> (a -> f (Maybe b)) -> s -> f t
witherOf = id
{-# INLINE witherOf #-}

-- | @'forMaybeOf' ≡ 'flip'@
forMaybeOf :: FilterLike f s t a b -> s -> (a -> f (Maybe b)) -> f t
forMaybeOf = flip
{-# INLINE forMaybeOf #-}

-- In case mapMaybeOf or filterOf is called with a function of
-- unknown arity, we don't want to slow things down to raise
-- its arity.
idDot :: (a -> b) -> a -> Identity b
idDot = coerce

-- | 'mapMaybe' through a filter.
mapMaybeOf :: FilterLike Identity s t a b -> (a -> Maybe b) -> s -> t
mapMaybeOf w f = runIdentity . w (idDot f)
{-# INLINE mapMaybeOf #-}

-- | 'catMaybes' through a filter.
catMaybesOf :: FilterLike Identity s t (Maybe a) a -> s -> t
catMaybesOf w = mapMaybeOf w id
{-# INLINE catMaybesOf #-}

-- | 'filterA' through a filter.
filterAOf :: Functor f => FilterLike' f s a -> (a -> f Bool) -> s -> f s
filterAOf w f = w $ \a -> (\b -> if b then Just a else Nothing) <$> f a
{-# INLINABLE filterAOf #-}

-- | Filter each element of a structure targeted by a 'Filter'.
filterOf :: FilterLike' Identity s a -> (a -> Bool) -> s -> s
filterOf w f = runIdentity . filterAOf w (idDot f)
{-# INLINE filterOf #-}

-- | Remove the duplicate elements through a filter.
ordNubOf :: Ord a => FilterLike' (State (Set.Set a)) s a -> s -> s
ordNubOf w = ordNubOnOf w id

-- | Remove the duplicate elements through a filter.
ordNubOnOf :: Ord b => FilterLike' (State (Set.Set b)) s a -> (a -> b) -> s -> s
ordNubOnOf w p t = evalState (w f t) Set.empty
  where
    f a = let b = p a in state $ \s -> if Set.member b s
      then (Nothing, s)
      else (Just a, Set.insert b s)
{-# INLINE ordNubOf #-}

-- | Remove the duplicate elements through a filter.
-- It is often faster than 'ordNubOf', especially when the comparison is expensive.
hashNubOf :: (Eq a, Hashable a) => FilterLike' (State (HSet.HashSet a)) s a -> s -> s
hashNubOf w = hashNubOnOf w id

-- | Remove the duplicate elements through a filter.
hashNubOnOf :: (Eq b, Hashable b) => FilterLike' (State (HSet.HashSet b)) s a -> (a -> b) -> s -> s
hashNubOnOf w p t = evalState (w f t) HSet.empty
  where
    f a = let b = p a in state $ \s -> if HSet.member b s
      then (Nothing, s)
      else (Just a, HSet.insert b s)
{-# INLINE hashNubOf #-}
