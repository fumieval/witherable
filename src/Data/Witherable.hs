{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, UndecidableInstances, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
module Data.Witherable
  ( Filterable(..)
  , Witherable(..)
  , witherM
  , blightM
  , ordNub
  , hashNub
  , forMaybe
  -- * Generalization
  , FilterLike, Filter, FilterLike', Filter'
  , witherOf
  , forMaybeOf
  , mapMaybeOf
  , catMaybesOf
  , filterAOf
  , filterOf
  , ordNubOf
  , hashNubOf
   -- * Cloning
  , cloneFilter
  , Peat(..)
  )

where
import qualified Data.Maybe as Maybe
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.HashSet as HSet
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Functor.Compose
import Data.Hashable
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Orphans ()
#if (MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif
import Prelude -- Fix redundant import warning

-- | This type allows combinators to take a 'Filter' specializing the parameter @f@.
type FilterLike f s t a b = (a -> f (Maybe b)) -> s -> f t

-- | A 'Filter' is like a <http://hackage.haskell.org/package/lens-4.13.2.1/docs/Control-Lens-Type.html#t:Traversal Traversal>,
-- but you can also remove targets.
type Filter s t a b = forall f. Applicative f => FilterLike f s t a b

-- | A simple 'FilterLike'.
type FilterLike' f s a = FilterLike f s s a a

-- | A simple 'Filter'.
type Filter' s a = forall f. Applicative f => FilterLike' f s a

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

-- | Reconstitute a 'Filter' from its monomorphic form.
cloneFilter :: FilterLike (Peat a b) s t a b -> Filter s t a b
cloneFilter l f = (`runPeat` f) . l (\a -> Peat $ \g -> g a)
{-# INLINABLE cloneFilter #-}

-- | 'witherOf' is actually 'id', but left for consistency.
witherOf :: FilterLike f s t a b -> (a -> f (Maybe b)) -> s -> f t
witherOf = id
{-# INLINE witherOf #-}

-- | @'forMaybeOf' ≡ 'flip'@
forMaybeOf :: FilterLike f s t a b -> s -> (a -> f (Maybe b)) -> f t
forMaybeOf = flip
{-# INLINE forMaybeOf #-}

-- | 'mapMaybe' through a filter.
mapMaybeOf :: FilterLike Identity s t a b -> (a -> Maybe b) -> s -> t
mapMaybeOf w f = runIdentity . w (Identity . f)
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
filterOf w f = runIdentity . filterAOf w (Identity . f)
{-# INLINE filterOf #-}

-- | Like 'Functor', but it include 'Maybe' effects.
--
-- Formally, the class 'Filterable' represents a functor from @Kleisli Maybe@ to @Hask@.
--
-- A definition of 'mapMaybe' must satisfy the following laws:
--
-- [/identity/]
--   @'mapMaybe' Just ≡ id@
--
-- [/conservation/]
--   @'mapMaybe' (Just . f) ≡ 'fmap' f@
--
-- [/composition/]
--   @'mapMaybe' f . 'mapMaybe' g ≡ 'mapMaybe' (f <=< g)@
class Functor f => Filterable f where
  -- | Like 'Maybe.mapMaybe'.
  mapMaybe :: (a -> Maybe b) -> f a -> f b
  mapMaybe f = catMaybes . fmap f
  {-# INLINE mapMaybe #-}

  -- | @'catMaybes' ≡ 'mapMaybe' 'id'@
  catMaybes :: f (Maybe a) -> f a
  catMaybes = mapMaybe id
  {-# INLINE catMaybes #-}

  -- | @'filter' f . 'filter' g ≡ filter ('liftA2' ('&&') f g)@
  filter :: (a -> Bool) -> f a -> f a
  filter f = mapMaybe $ \a -> if f a then Just a else Nothing
  {-# INLINE filter #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL mapMaybe | catMaybes #-}
#endif

-- | Like 'Traversable', but you can remove elements instead of updating them.
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/identity/]
--   @'wither' ('pure' . Just) ≡ 'pure'@
--
-- [/conservation/]
--   @'wither' ('fmap' 'Just' . f) ≡ 'traverse' f@
--
-- [/composition/]
--   @'Compose' . 'fmap' ('wither' f) . 'wither' g ≡ 'wither' ('Compose' . 'fmap' ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
--   @t . 'wither' f ≡ 'wither' (t . f)@
--

class (T.Traversable t, Filterable t) => Witherable t where

  -- | @'traverse' f ≡ 'wither' ('fmap' 'Just' . f)@
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f
  {-# INLINE wither #-}

  -- | @'Compose' . 'fmap' ('filterA' f) . 'filterA' g ≡ 'filterA' (\x -> 'Compose' $ 'fmap' (\b -> (b&&) <$> f x) (g x)@
  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA = filterAOf wither

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL #-}
#endif

-- | @'forMaybe' = 'flip' 'wither'@
forMaybe :: (Witherable t, Applicative f) => t a -> (a -> f (Maybe b)) -> f (t b)
forMaybe = flip wither
{-# INLINE forMaybe #-}

-- | A variant of `wither` that works on 'MaybeT'.
witherM :: (Witherable t, Monad m) => (a -> MaybeT m b) -> t a -> m (t b)
witherM f = unwrapMonad . wither (WrapMonad . runMaybeT . f)
{-# INLINE witherM #-}

-- | 'blightM' is 'witherM' with its arguments flipped.
blightM :: (Monad m, Witherable t) => t a -> (a -> MaybeT m b) -> m (t b)
blightM = flip witherM
{-# INLINE blightM #-}

-- | Remove the duplicate elements through a filter.
ordNubOf :: Ord a => FilterLike' (State (Set.Set a)) s a -> s -> s
ordNubOf w t = evalState (w f t) Set.empty
  where
    f a = state $ \s -> if Set.member a s
      then (Nothing, s)
      else (Just a, Set.insert a s)
{-# INLINE ordNubOf #-}

-- | Remove the duplicate elements through a filter.
-- It is often faster than 'ordNubOf', especially when the comparison is expensive.
hashNubOf :: (Eq a, Hashable a) => FilterLike' (State (HSet.HashSet a)) s a -> s -> s
hashNubOf w t = evalState (w f t) HSet.empty
  where
    f a = state $ \s -> if HSet.member a s
      then (Nothing, s)
      else (Just a, HSet.insert a s)
{-# INLINE hashNubOf #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is asymptotically faster than using
--   'Data.List.nub' from "Data.List".
ordNub :: (Witherable t, Ord a) => t a -> t a
ordNub = ordNubOf wither
{-# INLINE ordNub #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparison (like 'String').
hashNub :: (Witherable t, Eq a, Hashable a) => t a -> t a
hashNub = hashNubOf wither
{-# INLINE hashNub #-}

instance Filterable Maybe where
  mapMaybe f = (>>= f)
  {-# INLINE mapMaybe #-}

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

instance Monoid e => Filterable (Either e) where
  mapMaybe _ (Left e) = Left e
  mapMaybe f (Right a) = maybe (Left mempty) Right $ f a
  {-# INLINABLE mapMaybe #-}

instance Monoid e => Witherable (Either e) where
  wither _ (Left e) = pure (Left e)
  wither f (Right a) = fmap (maybe (Left mempty) Right) (f a)
  {-# INLINABLE wither #-}

instance Filterable [] where
  mapMaybe = Maybe.mapMaybe
  catMaybes = Maybe.catMaybes
  filter = Prelude.filter

instance Witherable [] where
  wither f = go where
    go (x:xs) = maybe id (:) <$> f x <*> go xs
    go [] = pure []
  {-# INLINE[0] wither #-}

instance Filterable IM.IntMap where
  mapMaybe = IM.mapMaybe
  filter = IM.filter

instance Witherable IM.IntMap where

instance Filterable (M.Map k) where
  mapMaybe = M.mapMaybe
  filter = M.filter

instance Witherable (M.Map k) where

instance (Eq k, Hashable k) => Filterable (HM.HashMap k) where
  mapMaybe = HM.mapMaybe
  filter = HM.filter

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where

#if (MIN_VERSION_base(4,7,0))
instance Filterable Proxy where
 mapMaybe _ Proxy = Proxy

instance Witherable Proxy where
  wither _ Proxy = pure Proxy
#endif

instance Filterable (Const r) where
  mapMaybe _ (Const r) = Const r
  {-# INLINABLE mapMaybe #-}

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Filterable V.Vector where
  mapMaybe f = V.fromList . mapMaybe f . V.toList
  {-# INLINABLE mapMaybe #-}

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

instance Filterable S.Seq where
  mapMaybe f = S.fromList . mapMaybe f . F.toList
  {-# INLINABLE mapMaybe #-}

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}

instance (Functor f, Filterable g) => Filterable (Compose f g) where
  mapMaybe f = Compose . fmap (mapMaybe f) . getCompose

instance (T.Traversable f, Witherable g) => Witherable (Compose f g) where
  wither f = fmap Compose . T.traverse (wither f) . getCompose

instance Functor f => Filterable (MaybeT f) where
  mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

instance (T.Traversable t) => Witherable (MaybeT t) where
  wither f = fmap MaybeT . T.traverse (wither f) . runMaybeT
