{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  , (<$?>)
  , (<&?>)
  , Witherable(..)
  , ordNub
  , hashNub
  , forMaybe
  -- * Indexed variants
  , FilterableWithIndex(..)
  , WitherableWithIndex(..)
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
import qualified Control.Lens as Lens
import qualified Data.Maybe as Maybe
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Map.Monoidal as MM
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import qualified Data.HashSet as HSet
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Functor.Compose
import Data.Functor.Product as P
import Data.Functor.Sum as Sum
import Control.Monad.Trans.Identity
import Data.Hashable
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Orphans ()
#if (MIN_VERSION_base(4,7,0))
import Data.Proxy
import Data.Void
#endif
import Data.Coerce (coerce)
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
#if MIN_VERSION_base(4,10,0)
  liftA2 f (Peat xs) (Peat ys) = Peat $ \h -> liftA2 f (xs h) (ys h)
  {-# INLINE liftA2 #-}
#endif

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

-- In case mapMaybeOf or filterOf is called with a function of
-- unknown arity, we don't want to slow things down to raise
-- its arity.
idDot :: (a -> b) -> a -> Identity b
#if __GLASGOW_HASKELL__ >= 708
idDot = coerce
#else
idDot = (Identity .)
#endif

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

-- | Like 'Functor', but you can remove elements instead of updating them.
--
-- Formally, the class 'Filterable' represents a functor from @Kleisli Maybe@ to @Hask@.
--
-- A definition of 'mapMaybe' must satisfy the following laws:
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

-- | Indexed variant of 'Filterable'.
class (Lens.FunctorWithIndex i t, Filterable t) => FilterableWithIndex i t | t -> i where
  imapMaybe :: (i -> a -> Maybe b) -> t a -> t b
  imapMaybe f = catMaybes . Lens.imap f
  {-# INLINE imapMaybe #-}

  -- | @'ifilter' f . 'ifilter' g ≡ ifilter (\i -> 'liftA2' ('&&') (f i) (g i))@
  ifilter :: (i -> a -> Bool) -> t a -> t a
  ifilter f = imapMaybe $ \i a -> if f i a then Just a else Nothing
  {-# INLINE ifilter #-}

-- | An enhancement of 'Traversable' with 'Filterable'
--
-- A definition of 'wither' must satisfy the following laws:
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

  -- | Effectful 'mapMaybe'.
  --
  -- @'wither' ('pure' . f) ≡ 'pure' . 'mapMaybe' f@
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f
  {-# INLINE wither #-}

  -- | @Monadic variant of 'wither'. This may have more efficient implementation.@
  witherM :: Monad m => (a -> m (Maybe b)) -> t a -> m (t b)
#if MIN_VERSION_base(4,8,0)
  witherM = wither
#elif __GLASGOW_HASKELL__ >= 708
  witherM f = unwrapMonad . wither (coerce f)
#else
  witherM f = unwrapMonad . wither (WrapMonad . f)
#endif
  {-# INLINE witherM #-}

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA = filterAOf wither

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL #-}
#endif

-- | @'forMaybe' = 'flip' 'wither'@
forMaybe :: (Witherable t, Applicative f) => t a -> (a -> f (Maybe b)) -> f (t b)
forMaybe = flip wither
{-# INLINE forMaybe #-}

-- | Indexed variant of 'Witherable'.
class (Lens.TraversableWithIndex i t, Witherable t) => WitherableWithIndex i t | t -> i where
  -- | Effectful 'imapMaybe'.
  --
  -- @'iwither' (\ i -> 'pure' . f i) ≡ 'pure' . 'imapMaybe' f@
  iwither :: (Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
  iwither f = fmap catMaybes . Lens.itraverse f

  -- | @Monadic variant of 'wither'. This may have more efficient implementation.@
  iwitherM :: (Monad m) => (i -> a -> m (Maybe b)) -> t a -> m (t b)
#if MIN_VERSION_base(4,8,0)
  iwitherM = iwither
#elif __GLASGOW_HASKELL__ >= 708
  iwitherM f = unwrapMonad . iwither (coerce f)
#else
  iwitherM f = unwrapMonad . iwither (\i -> WrapMonad . f i)
#endif

  ifilterA :: (Applicative f) => (i -> a -> f Bool) -> t a -> f (t a)
  ifilterA f = iwither (\i a -> (\b -> if b then Just a else Nothing) <$> f i a)

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
ordNub = ordNubOf witherM
{-# INLINE ordNub #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparison (like 'String').
hashNub :: (Witherable t, Eq a, Hashable a) => t a -> t a
hashNub = hashNubOf witherM
{-# INLINE hashNub #-}

-- | A default implementation for 'mapMaybe'.
mapMaybeDefault :: (Foldable f, Alternative f) => (a -> Maybe b) -> f a -> f b
mapMaybeDefault p = foldr (\x xs -> case p x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE mapMaybeDefault #-}

-- | A default implementation for 'imapMaybe'.
imapMaybeDefault :: (Lens.FoldableWithIndex i f, Alternative f) => (i -> a -> Maybe b) -> f a -> f b
imapMaybeDefault p = Lens.ifoldr (\i x xs -> case p i x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE imapMaybeDefault #-}

newtype WrappedFilterable f a = WrapFilterable {unwrapFilterable :: f a} 
  deriving (Functor, Foldable, Traversable, Applicative, Alternative)

instance (Lens.FunctorWithIndex i f) => Lens.FunctorWithIndex i (WrappedFilterable f) where
  imap f = WrapFilterable . Lens.imap f . unwrapFilterable

instance (Lens.FoldableWithIndex i f) => Lens.FoldableWithIndex i (WrappedFilterable f) where
  ifoldMap f = Lens.ifoldMap f . unwrapFilterable

instance (Lens.TraversableWithIndex i f) => Lens.TraversableWithIndex i (WrappedFilterable f) where
  itraverse f = fmap WrapFilterable . Lens.itraverse f . unwrapFilterable

instance (Foldable f, Alternative f) => Filterable (WrappedFilterable f) where
    {-#INLINE mapMaybe#-}
    mapMaybe = mapMaybeDefault

instance (Lens.FunctorWithIndex i f, Lens.FoldableWithIndex i f, Alternative f) => FilterableWithIndex i (WrappedFilterable f) where
  {-# INLINE imapMaybe #-}
  imapMaybe = imapMaybeDefault

instance Filterable Maybe where
  mapMaybe f = (>>= f)
  {-# INLINE mapMaybe #-}

instance FilterableWithIndex () Maybe

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

instance WitherableWithIndex () Maybe

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

instance FilterableWithIndex Int []

instance Witherable [] where
  wither f = go where
    go (x:xs) = liftA2 (maybe id (:)) (f x) (go xs)
    go [] = pure []
  {-# INLINE[0] wither #-}

instance WitherableWithIndex Int []

instance Filterable IM.IntMap where
  mapMaybe = IM.mapMaybe
  filter = IM.filter

instance FilterableWithIndex Int IM.IntMap where
  imapMaybe = IM.mapMaybeWithKey
  ifilter = IM.filterWithKey

instance Witherable IM.IntMap where

instance WitherableWithIndex Int IM.IntMap where

instance Filterable (M.Map k) where
  mapMaybe = M.mapMaybe
  filter = M.filter

instance FilterableWithIndex k (M.Map k) where
  imapMaybe = M.mapMaybeWithKey
  ifilter = M.filterWithKey

instance Witherable (M.Map k) where
#if MIN_VERSION_containers(0,5,8)
  wither f = M.traverseMaybeWithKey (const f)
#endif

instance WitherableWithIndex k (M.Map k) where
#if MIN_VERSION_containers(0,5,8)
  iwither = M.traverseMaybeWithKey
#endif

instance Filterable (MM.MonoidalMap k) where
  mapMaybe = MM.mapMaybe
  filter = MM.filter

instance FilterableWithIndex k (MM.MonoidalMap k) where
  imapMaybe = MM.mapMaybeWithKey
  ifilter = MM.filterWithKey

instance Witherable (MM.MonoidalMap k)

instance WitherableWithIndex k (MM.MonoidalMap k)

instance (Eq k, Hashable k) => Filterable (HM.HashMap k) where
  mapMaybe = HM.mapMaybe
  filter = HM.filter

instance (Eq k, Hashable k) => FilterableWithIndex k (HM.HashMap k) where
  imapMaybe = HM.mapMaybeWithKey
  ifilter = HM.filterWithKey

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where

instance (Eq k, Hashable k) => WitherableWithIndex k (HM.HashMap k) where
  
#if (MIN_VERSION_base(4,7,0))
instance Filterable Proxy where
 mapMaybe _ Proxy = Proxy

instance FilterableWithIndex Void Proxy

instance Witherable Proxy where
  wither _ Proxy = pure Proxy

instance WitherableWithIndex Void Proxy
#endif

instance Filterable (Const r) where
  mapMaybe _ (Const r) = Const r
  {-# INLINABLE mapMaybe #-}

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Filterable V.Vector where
  mapMaybe = V.mapMaybe

instance FilterableWithIndex Int V.Vector where
  imapMaybe = V.imapMaybe
  ifilter = V.ifilter

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

instance WitherableWithIndex Int V.Vector

instance Filterable S.Seq where
  mapMaybe f = S.fromList . mapMaybe f . F.toList
  {-# INLINABLE mapMaybe #-}

instance FilterableWithIndex Int S.Seq

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}

instance WitherableWithIndex Int S.Seq

-- The instances for Compose, Product, and Sum are not entirely
-- unique. Any particular composition, product, or sum of functors
-- may support a variety of 'wither' implementations.

instance (Functor f, Filterable g) => Filterable (Compose f g) where
  mapMaybe f = Compose . fmap (mapMaybe f) . getCompose

instance (Lens.FunctorWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (i, j) (Compose f g) where
  imapMaybe f = Compose . Lens.imap (\i -> imapMaybe (\j -> f (i, j))) . getCompose

instance (T.Traversable f, Witherable g) => Witherable (Compose f g) where
  wither f = fmap Compose . T.traverse (wither f) . getCompose

instance (Lens.TraversableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (i, j) (Compose f g) where
  iwither f = fmap Compose . Lens.itraverse (\i -> iwither (\j -> f (i, j))) . getCompose

instance (Filterable f, Filterable g) => Filterable (P.Product f g) where
  mapMaybe f (P.Pair x y) = P.Pair (mapMaybe f x) (mapMaybe f y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (P.Product f g) where
  imapMaybe f (P.Pair x y) = P.Pair (imapMaybe (f . Left) x) (imapMaybe (f . Right) y)

instance (Witherable f, Witherable g) => Witherable (P.Product f g) where
  wither f (P.Pair x y) = liftA2 P.Pair (wither f x) (wither f y)

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (P.Product f g) where
  iwither f (P.Pair x y) = P.Pair <$> iwither (f . Left) x <*> iwither (f . Right) y

instance (Filterable f, Filterable g) => Filterable (Sum.Sum f g) where
  mapMaybe f (Sum.InL x) = Sum.InL (mapMaybe f x)
  mapMaybe f (Sum.InR y) = Sum.InR (mapMaybe f y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (Sum.Sum f g) where
  imapMaybe f (Sum.InL x) = Sum.InL (imapMaybe (f . Left) x)
  imapMaybe f (Sum.InR y) = Sum.InR (imapMaybe (f . Right) y)

instance (Witherable f, Witherable g) => Witherable (Sum.Sum f g) where
  wither f (Sum.InL x) = Sum.InL <$> wither f x
  wither f (Sum.InR y) = Sum.InR <$> wither f y

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (Sum.Sum f g) where
  iwither f (Sum.InL x) = Sum.InL <$> iwither (f . Left) x
  iwither f (Sum.InR y) = Sum.InR <$> iwither (f . Right) y

instance Filterable f => Filterable (IdentityT f) where
  mapMaybe f (IdentityT m) = IdentityT (mapMaybe f m)

instance (FilterableWithIndex i f) => FilterableWithIndex i (IdentityT f) where
  imapMaybe f (IdentityT m) = IdentityT (imapMaybe f m)

instance Witherable f => Witherable (IdentityT f) where
  wither f (IdentityT m) = IdentityT <$> wither f m

instance (WitherableWithIndex i f) => WitherableWithIndex i (IdentityT f) where
  iwither f (IdentityT m) = IdentityT <$> iwither f m

instance Functor f => Filterable (MaybeT f) where
  mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

instance (T.Traversable t) => Witherable (MaybeT t) where
  wither f = fmap MaybeT . T.traverse (wither f) . runMaybeT
