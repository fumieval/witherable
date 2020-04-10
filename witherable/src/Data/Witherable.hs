{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
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
import Data.Witherable.Class
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
import Data.Functor.Reverse (Reverse (..))
import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Trans.State.Strict
import Data.Monoid
import Data.Orphans ()
import Data.Proxy
import Data.Void
import Data.Coerce (coerce)
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
  iwitherM = iwither

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

instance FilterableWithIndex () Maybe

instance WitherableWithIndex () Maybe

-- Option doesn't have the necessary instances in Lens
--instance FilterableWithIndex () Option
--instance WitherableWithIndex () Option

instance FilterableWithIndex Int []

instance FilterableWithIndex Int ZipList

instance (Alternative f, T.Traversable f) => Witherable (WrappedFoldable f)

instance WitherableWithIndex Int []

instance WitherableWithIndex Int ZipList

instance FilterableWithIndex Int IM.IntMap where
  imapMaybe = IM.mapMaybeWithKey
  ifilter = IM.filterWithKey

instance WitherableWithIndex Int IM.IntMap where

instance FilterableWithIndex k (M.Map k) where
  imapMaybe = M.mapMaybeWithKey
  ifilter = M.filterWithKey

instance WitherableWithIndex k (M.Map k) where
#if MIN_VERSION_containers(0,5,8)
  iwither = M.traverseMaybeWithKey
#endif

instance Filterable (MM.MonoidalMap k) where
  mapMaybe = MM.mapMaybe
  filter = MM.filter

instance Witherable (MM.MonoidalMap k)

instance FilterableWithIndex k (MM.MonoidalMap k) where
  imapMaybe = MM.mapMaybeWithKey
  ifilter = MM.filterWithKey

instance WitherableWithIndex k (MM.MonoidalMap k)

instance (Eq k, Hashable k) => FilterableWithIndex k (HM.HashMap k) where
  imapMaybe = HM.mapMaybeWithKey
  ifilter = HM.filterWithKey

instance (Eq k, Hashable k) => WitherableWithIndex k (HM.HashMap k) where

instance FilterableWithIndex Void Proxy

instance WitherableWithIndex Void Proxy

instance FilterableWithIndex Int V.Vector where
  imapMaybe = V.imapMaybe
  ifilter = V.ifilter

instance WitherableWithIndex Int V.Vector

instance FilterableWithIndex Int S.Seq

instance WitherableWithIndex Int S.Seq

instance (Lens.FunctorWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (i, j) (Compose f g) where
  imapMaybe f = Compose . Lens.imap (\i -> imapMaybe (\j -> f (i, j))) . getCompose
  ifilter p = Compose . Lens.imap (\i -> ifilter (\j -> p (i, j))) . getCompose

instance (Lens.TraversableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (i, j) (Compose f g) where
  iwither f = fmap Compose . Lens.itraverse (\i -> iwither (\j -> f (i, j))) . getCompose
  iwitherM f = fmap Compose . Lens.imapM (\i -> iwitherM (\j -> f (i, j))) . getCompose
  ifilterA p = fmap Compose . Lens.itraverse (\i -> ifilterA (\j -> p (i, j))) . getCompose

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (P.Product f g) where
  imapMaybe f (P.Pair x y) = P.Pair (imapMaybe (f . Left) x) (imapMaybe (f . Right) y)
  ifilter p (P.Pair x y) = P.Pair (ifilter (p . Left) x) (ifilter (p . Right) y)

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (P.Product f g) where
  iwither f (P.Pair x y) = liftA2 P.Pair (iwither (f . Left) x) (iwither (f . Right) y)
  iwitherM f (P.Pair x y) = liftA2 P.Pair (iwitherM (f . Left) x) (iwitherM (f . Right) y)
  ifilterA p (P.Pair x y) = liftA2 P.Pair (ifilterA (p . Left) x) (ifilterA (p . Right) y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (Sum.Sum f g) where
  imapMaybe f (Sum.InL x) = Sum.InL (imapMaybe (f . Left) x)
  imapMaybe f (Sum.InR y) = Sum.InR (imapMaybe (f . Right) y)

  ifilter f (Sum.InL x) = Sum.InL (ifilter (f . Left) x)
  ifilter f (Sum.InR y) = Sum.InR (ifilter (f . Right) y)

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (Sum.Sum f g) where
  iwither f (Sum.InL x) = Sum.InL <$> iwither (f . Left) x
  iwither f (Sum.InR y) = Sum.InR <$> iwither (f . Right) y

  iwitherM f (Sum.InL x) = Sum.InL <$> iwitherM (f . Left) x
  iwitherM f (Sum.InR y) = Sum.InR <$> iwitherM (f . Right) y

  ifilterA f (Sum.InL x) = Sum.InL <$> ifilterA (f . Left) x
  ifilterA f (Sum.InR y) = Sum.InR <$> ifilterA (f . Right) y

deriving instance (FilterableWithIndex i f) => FilterableWithIndex i (IdentityT f)

instance (WitherableWithIndex i f) => WitherableWithIndex i (IdentityT f) where
  iwither f (IdentityT m) = IdentityT <$> iwither f m
  iwitherM f (IdentityT m) = IdentityT <$> iwitherM f m
  ifilterA p (IdentityT m) = IdentityT <$> ifilterA p m

deriving instance FilterableWithIndex i t => FilterableWithIndex i (Reverse t)

-- | Wither from right to left.
instance WitherableWithIndex i t => WitherableWithIndex i (Reverse t) where
  iwither f (Reverse t) = fmap Reverse . forwards $ iwither (\i -> Backwards . f i) t
  -- We can't do anything special with iwitherM, because Backwards m is not
  -- generally a Monad.
  ifilterA p (Reverse t) = fmap Reverse . forwards $ ifilterA (\i -> Backwards . p i) t

deriving instance FilterableWithIndex i t => FilterableWithIndex i (Backwards t)

instance WitherableWithIndex i t => WitherableWithIndex i (Backwards t) where
  iwither f (Backwards xs) = Backwards <$> iwither f xs
  iwitherM f (Backwards xs) = Backwards <$> iwitherM f xs
  ifilterA f (Backwards xs) = Backwards <$> ifilterA f xs
