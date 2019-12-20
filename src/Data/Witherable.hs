{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
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
  , WitherLike, Wither, WitherLike', Wither'
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
  -- * Wrapper
  , WrappedFoldable(..)
  )

where
import qualified Control.Lens as Lens
import qualified Data.Maybe as Maybe
import Data.Bool (bool)
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Map.Monoidal as MM
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import qualified Data.HashSet as HSet
import qualified GHC.Generics as Generics
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Functor.Compose
import Data.Functor.Product as P
import Data.Functor.Sum as Sum
import Control.Monad.Trans.Identity
import Data.Hashable
import Data.Functor.Identity
import Data.Functor.Reverse (Reverse (..))
import Control.Applicative.Backwards (Backwards (..))
import Data.Semigroup (Option (..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Orphans ()
#if (MIN_VERSION_base(4,7,0))
import Data.Proxy
import Data.Void
#endif
import Data.Coerce (coerce)
import qualified Prelude
import Prelude hiding (filter)

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
mapMaybeDefault :: (F.Foldable f, Alternative f) => (a -> Maybe b) -> f a -> f b
mapMaybeDefault p = F.foldr (\x xs -> case p x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE mapMaybeDefault #-}

-- | A default implementation for 'imapMaybe'.
imapMaybeDefault :: (Lens.FoldableWithIndex i f, Alternative f) => (i -> a -> Maybe b) -> f a -> f b
imapMaybeDefault p = Lens.ifoldr (\i x xs -> case p i x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE imapMaybeDefault #-}

newtype WrappedFoldable f a = WrapFilterable {unwrapFoldable :: f a}
  deriving (Functor, F.Foldable, T.Traversable, Applicative, Alternative)

instance (Lens.FunctorWithIndex i f) => Lens.FunctorWithIndex i (WrappedFoldable f) where
  imap f = WrapFilterable . Lens.imap f . unwrapFoldable

instance (Lens.FoldableWithIndex i f) => Lens.FoldableWithIndex i (WrappedFoldable f) where
  ifoldMap f = Lens.ifoldMap f . unwrapFoldable

instance (Lens.TraversableWithIndex i f) => Lens.TraversableWithIndex i (WrappedFoldable f) where
  itraverse f = fmap WrapFilterable . Lens.itraverse f . unwrapFoldable

instance (F.Foldable f, Alternative f) => Filterable (WrappedFoldable f) where
    {-#INLINE mapMaybe#-}
    mapMaybe = mapMaybeDefault

instance (Lens.FunctorWithIndex i f, Lens.FoldableWithIndex i f, Alternative f) => FilterableWithIndex i (WrappedFoldable f) where
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

instance Filterable Option where
  mapMaybe f = (>>= Option . f)
  {-# INLINE mapMaybe #-}

instance Witherable Option where
  wither f (Option x) = Option <$> wither f x
  {-# INLINE wither #-}

-- Option doesn't have the necessary instances in Lens
--instance FilterableWithIndex () Option
--instance WitherableWithIndex () Option

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

instance Filterable ZipList where
  mapMaybe f = ZipList . Maybe.mapMaybe f . getZipList
  catMaybes = ZipList . Maybe.catMaybes . getZipList
  filter f = ZipList . Prelude.filter f . getZipList

instance FilterableWithIndex Int ZipList

instance (Alternative f, T.Traversable f) => Witherable (WrappedFoldable f)

-- | Methods are good consumers for fusion.
instance Witherable [] where
  wither f = foldr go (pure []) where
    go x r = liftA2 (maybe id (:)) (f x) r
  {-# INLINE wither #-}
  witherM f = foldr go (pure []) where
    go x r = f x >>=
      (\z -> case z of
        Nothing -> r
        Just y -> ((:) y) <$> r
      )
  {-# INLINE witherM #-}

  -- Compared to the default, this fuses an fmap into a liftA2.
  filterA p = go where
    go (x:xs) = liftA2 (bool id (x :)) (p x) (go xs)
    go [] = pure []

instance WitherableWithIndex Int []

instance Witherable ZipList where
  wither f = fmap ZipList . wither f . getZipList

instance WitherableWithIndex Int ZipList

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
  filter = S.filter

instance FilterableWithIndex Int S.Seq

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}

{-
  -- TODO: try to figure out whether the following is better or worse for
  -- typical applications. It builds the sequence incrementally rather than
  -- building a list and converting.  This is basically the same approach
  -- currently used by Data.Sequence.filter.

  witherM f = F.foldlM go S.empty
    where
      --go :: S.Seq b -> a -> m (S.Seq b)
      go s a = do
        mb <- f a
        case mb of
          Nothing -> pure s
          Just b -> pure $! s S.|> b
  {-# INLINABLE witherM #-}
-}

instance WitherableWithIndex Int S.Seq

-- The instances for Compose, Product, and Sum are not entirely
-- unique. Any particular composition, product, or sum of functors
-- may support a variety of 'wither' implementations.

instance (Functor f, Filterable g) => Filterable (Compose f g) where
  mapMaybe f = Compose . fmap (mapMaybe f) . getCompose
  filter p = Compose . fmap (filter p) . getCompose
  catMaybes = Compose . fmap catMaybes . getCompose

instance (Lens.FunctorWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (i, j) (Compose f g) where
  imapMaybe f = Compose . Lens.imap (\i -> imapMaybe (\j -> f (i, j))) . getCompose
  ifilter p = Compose . Lens.imap (\i -> ifilter (\j -> p (i, j))) . getCompose

instance (T.Traversable f, Witherable g) => Witherable (Compose f g) where
  wither f = fmap Compose . T.traverse (wither f) . getCompose
  witherM f = fmap Compose . T.mapM (witherM f) . getCompose
  filterA p = fmap Compose . T.traverse (filterA p) . getCompose

instance (Lens.TraversableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (i, j) (Compose f g) where
  iwither f = fmap Compose . Lens.itraverse (\i -> iwither (\j -> f (i, j))) . getCompose
  iwitherM f = fmap Compose . Lens.imapM (\i -> iwitherM (\j -> f (i, j))) . getCompose
  ifilterA p = fmap Compose . Lens.itraverse (\i -> ifilterA (\j -> p (i, j))) . getCompose

instance (Filterable f, Filterable g) => Filterable (P.Product f g) where
  mapMaybe f (P.Pair x y) = P.Pair (mapMaybe f x) (mapMaybe f y)
  filter p (P.Pair x y) = P.Pair (filter p x) (filter p y)
  catMaybes (P.Pair x y) = P.Pair (catMaybes x) (catMaybes y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (P.Product f g) where
  imapMaybe f (P.Pair x y) = P.Pair (imapMaybe (f . Left) x) (imapMaybe (f . Right) y)
  ifilter p (P.Pair x y) = P.Pair (ifilter (p . Left) x) (ifilter (p . Right) y)

instance (Witherable f, Witherable g) => Witherable (P.Product f g) where
  wither f (P.Pair x y) = liftA2 P.Pair (wither f x) (wither f y)
  witherM f (P.Pair x y) = liftA2 P.Pair (witherM f x) (witherM f y)
  filterA p (P.Pair x y) = liftA2 P.Pair (filterA p x) (filterA p y)

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (P.Product f g) where
  iwither f (P.Pair x y) = liftA2 P.Pair (iwither (f . Left) x) (iwither (f . Right) y)
  iwitherM f (P.Pair x y) = liftA2 P.Pair (iwitherM (f . Left) x) (iwitherM (f . Right) y)
  ifilterA p (P.Pair x y) = liftA2 P.Pair (ifilterA (p . Left) x) (ifilterA (p . Right) y)

instance (Filterable f, Filterable g) => Filterable (Sum.Sum f g) where
  mapMaybe f (Sum.InL x) = Sum.InL (mapMaybe f x)
  mapMaybe f (Sum.InR y) = Sum.InR (mapMaybe f y)

  catMaybes (Sum.InL x) = Sum.InL (catMaybes x)
  catMaybes (Sum.InR y) = Sum.InR (catMaybes y)

  filter p (Sum.InL x) = Sum.InL (filter p x)
  filter p (Sum.InR y) = Sum.InR (filter p y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (Sum.Sum f g) where
  imapMaybe f (Sum.InL x) = Sum.InL (imapMaybe (f . Left) x)
  imapMaybe f (Sum.InR y) = Sum.InR (imapMaybe (f . Right) y)

  ifilter f (Sum.InL x) = Sum.InL (ifilter (f . Left) x)
  ifilter f (Sum.InR y) = Sum.InR (ifilter (f . Right) y)

instance (Witherable f, Witherable g) => Witherable (Sum.Sum f g) where
  wither f (Sum.InL x) = Sum.InL <$> wither f x
  wither f (Sum.InR y) = Sum.InR <$> wither f y

  witherM f (Sum.InL x) = Sum.InL <$> witherM f x
  witherM f (Sum.InR y) = Sum.InR <$> witherM f y

  filterA f (Sum.InL x) = Sum.InL <$> filterA f x
  filterA f (Sum.InR y) = Sum.InR <$> filterA f y

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (Sum.Sum f g) where
  iwither f (Sum.InL x) = Sum.InL <$> iwither (f . Left) x
  iwither f (Sum.InR y) = Sum.InR <$> iwither (f . Right) y

  iwitherM f (Sum.InL x) = Sum.InL <$> iwitherM (f . Left) x
  iwitherM f (Sum.InR y) = Sum.InR <$> iwitherM (f . Right) y

  ifilterA f (Sum.InL x) = Sum.InL <$> ifilterA (f . Left) x
  ifilterA f (Sum.InR y) = Sum.InR <$> ifilterA (f . Right) y

deriving instance Filterable f => Filterable (IdentityT f)

deriving instance (FilterableWithIndex i f) => FilterableWithIndex i (IdentityT f)

instance Witherable f => Witherable (IdentityT f) where
  wither f (IdentityT m) = IdentityT <$> wither f m
  witherM f (IdentityT m) = IdentityT <$> witherM f m
  filterA p (IdentityT m) = IdentityT <$> filterA p m

instance (WitherableWithIndex i f) => WitherableWithIndex i (IdentityT f) where
  iwither f (IdentityT m) = IdentityT <$> iwither f m
  iwitherM f (IdentityT m) = IdentityT <$> iwitherM f m
  ifilterA p (IdentityT m) = IdentityT <$> ifilterA p m

instance Functor f => Filterable (MaybeT f) where
  mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

instance (T.Traversable t) => Witherable (MaybeT t) where
  wither f = fmap MaybeT . T.traverse (wither f) . runMaybeT
  witherM f = fmap MaybeT . T.mapM (wither f) . runMaybeT

deriving instance Filterable t => Filterable (Reverse t)

deriving instance FilterableWithIndex i t => FilterableWithIndex i (Reverse t)

-- | Wither from right to left.
instance Witherable t => Witherable (Reverse t) where
  wither f (Reverse t) =
    fmap Reverse . forwards $ wither (coerce f) t
  -- We can't do anything special with witherM, because Backwards m is not
  -- generally a Monad.
  filterA f (Reverse t) =
    fmap Reverse . forwards $ filterA (coerce f) t

-- | Wither from right to left.
instance WitherableWithIndex i t => WitherableWithIndex i (Reverse t) where
  iwither f (Reverse t) = fmap Reverse . forwards $ iwither (\i -> Backwards . f i) t
  -- We can't do anything special with iwitherM, because Backwards m is not
  -- generally a Monad.
  ifilterA p (Reverse t) = fmap Reverse . forwards $ ifilterA (\i -> Backwards . p i) t

deriving instance Filterable t => Filterable (Backwards t)
deriving instance FilterableWithIndex i t => FilterableWithIndex i (Backwards t)

instance Witherable t => Witherable (Backwards t) where
  wither f (Backwards xs) = Backwards <$> wither f xs
  witherM f (Backwards xs) = Backwards <$> witherM f xs
  filterA f (Backwards xs) = Backwards <$> filterA f xs

instance WitherableWithIndex i t => WitherableWithIndex i (Backwards t) where
  iwither f (Backwards xs) = Backwards <$> iwither f xs
  iwitherM f (Backwards xs) = Backwards <$> iwitherM f xs
  ifilterA f (Backwards xs) = Backwards <$> ifilterA f xs

#if __GLASGOW_HASKELL__ >= 708
instance Filterable Generics.V1 where
  mapMaybe _ v = case v of {}
  catMaybes v = case v of {}
  filter _ v = case v of {}

instance Witherable Generics.V1 where
  wither _ v = pure $ case v of {}
  filterA _ v = pure $ case v of {}
#endif

instance Filterable Generics.U1 where
  mapMaybe _ _ = Generics.U1
  catMaybes _ = Generics.U1
  filter _ _ = Generics.U1

instance Witherable Generics.U1 where
  wither _ _ = pure Generics.U1
  filterA _ _ = pure Generics.U1

instance Filterable f => Filterable (Generics.Rec1 f) where
  mapMaybe f (Generics.Rec1 a) = Generics.Rec1 (mapMaybe f a)
  catMaybes (Generics.Rec1 a) = Generics.Rec1 (catMaybes a)
  filter f (Generics.Rec1 a) = Generics.Rec1 (filter f a)

instance Witherable f => Witherable (Generics.Rec1 f) where
  wither f (Generics.Rec1 a) = fmap Generics.Rec1 (wither f a)
  witherM f (Generics.Rec1 a) = fmap Generics.Rec1 (witherM f a)
  filterA f (Generics.Rec1 a) = fmap Generics.Rec1 (filterA f a)

instance Filterable f => Filterable (Generics.M1 i c f) where
  mapMaybe f (Generics.M1 a) = Generics.M1 (mapMaybe f a)
  catMaybes (Generics.M1 a) = Generics.M1 (catMaybes a)
  filter f (Generics.M1 a) = Generics.M1 (filter f a)

instance Witherable f => Witherable (Generics.M1 i c f) where
  wither f (Generics.M1 a) = fmap Generics.M1 (wither f a)
  witherM f (Generics.M1 a) = fmap Generics.M1 (witherM f a)
  filterA f (Generics.M1 a) = fmap Generics.M1 (filterA f a)

instance (Filterable f, Filterable g) => Filterable ((Generics.:*:) f g) where
  mapMaybe f (a Generics.:*: b) = mapMaybe f a Generics.:*: mapMaybe f b
  catMaybes (a Generics.:*: b) = catMaybes a Generics.:*: catMaybes b
  filter f (a Generics.:*: b) = filter f a Generics.:*: filter f b

instance (Witherable f, Witherable g) => Witherable ((Generics.:*:) f g) where
  wither f (a Generics.:*: b) = liftA2 (Generics.:*:) (wither f a) (wither f b)
  witherM f (a Generics.:*: b) = liftA2 (Generics.:*:) (witherM f a) (witherM f b)
  filterA f (a Generics.:*: b) = liftA2 (Generics.:*:) (filterA f a) (filterA f b)

instance (Filterable f, Filterable g) => Filterable ((Generics.:+:) f g) where
  mapMaybe f (Generics.L1 a) = Generics.L1 (mapMaybe f a)
  mapMaybe f (Generics.R1 a) = Generics.R1 (mapMaybe f a)
  catMaybes (Generics.L1 a) = Generics.L1 (catMaybes a)
  catMaybes (Generics.R1 a) = Generics.R1 (catMaybes a)
  filter f (Generics.L1 a) = Generics.L1 (filter f a)
  filter f (Generics.R1 a) = Generics.R1 (filter f a)

instance (Witherable f, Witherable g) => Witherable ((Generics.:+:) f g) where
  wither f (Generics.L1 a) = fmap Generics.L1 (wither f a)
  wither f (Generics.R1 a) = fmap Generics.R1 (wither f a)
  witherM f (Generics.L1 a) = fmap Generics.L1 (witherM f a)
  witherM f (Generics.R1 a) = fmap Generics.R1 (witherM f a)
  filterA f (Generics.L1 a) = fmap Generics.L1 (filterA f a)
  filterA f (Generics.R1 a) = fmap Generics.R1 (filterA f a)

instance (Functor f, Filterable g) => Filterable ((Generics.:.:) f g) where
  mapMaybe f = Generics.Comp1 . fmap (mapMaybe f) . Generics.unComp1
  catMaybes = Generics.Comp1 . fmap catMaybes . Generics.unComp1
  filter f = Generics.Comp1 . fmap (filter f) . Generics.unComp1

instance (T.Traversable f, Witherable g) => Witherable ((Generics.:.:) f g) where
  wither f = fmap Generics.Comp1 . T.traverse (wither f) . Generics.unComp1
  witherM f = fmap Generics.Comp1 . T.mapM (witherM f) . Generics.unComp1
  filterA f = fmap Generics.Comp1 . T.traverse (filterA f) . Generics.unComp1
