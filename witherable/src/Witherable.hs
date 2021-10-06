{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE Trustworthy #-}
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
  , ordNubOn
  , hashNub
  , hashNubOn
  , forMaybe
  -- * Indexed variants
  , FilterableWithIndex(..)
  , WitherableWithIndex(..)
  -- * Wrapper
  , WrappedFoldable(..)
  )

where

import Control.Applicative
import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy (evalState, state)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable.WithIndex
import Data.Functor.Compose
import Data.Functor.Product as P
import Data.Functor.Reverse (Reverse (..))
import Data.Functor.Sum as Sum
import Data.Functor.WithIndex
import Data.Functor.WithIndex.Instances ()
import Data.Hashable
import Data.Monoid
import Data.Orphans ()
import Data.Proxy
#if !MIN_VERSION_base(4,16,0)
import Data.Semigroup (Option (..))
#endif
import Data.Traversable.WithIndex
import Data.Void
import Prelude hiding (filter)
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HSet
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified GHC.Generics as Generics
import qualified Prelude

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

  -- | @'filter' f . 'filter' g ≡ filter ('liftA2' ('&&') g f)@
  filter :: (a -> Bool) -> f a -> f a
  filter f = mapMaybe $ \a -> if f a then Just a else Nothing
  {-# INLINE filter #-}

  {-# MINIMAL mapMaybe | catMaybes #-}

-- | An enhancement of 'Traversable' with 'Filterable'
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/identity/]
--   @'wither' ('Data.Functor.Identity' . Just) ≡ 'Data.Functor.Identity'@
--
-- [/composition/]
--   @'Compose' . 'fmap' ('wither' f) . 'wither' g ≡ 'wither' ('Compose' . 'fmap' ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
-- [/naturality/]
--   @t . 'wither' f ≡ 'wither' (t . f)@
--
--     Where @t@ is an //applicative transformation// in the sense described in the
--     'Traversable' documentation.
-- 
-- In the relation to superclasses, these should satisfy too:
--
-- [/conservation/]
--    @'wither' ('fmap' Just . f) = 'T.traverse' f@
--
-- [/pure filter/]
--    @'wither' ('Data.Functor.Identity' . f) = 'Data.Functor.Identity' . 'mapMaybe' f@
-- 
-- See the @Properties.md@ and @Laws.md@ files in the git distribution for more
-- in-depth explanation about properties of @Witherable@ containers.
--
-- The laws and restrictions are enough to
-- constrain @'wither'@ to be uniquely determined as the following default implementation.
-- 
-- @wither f = fmap 'catMaybes' . 'T.traverse' f@
-- 
-- If not to provide better-performing implementation,
-- it's not necessary to implement any one method of
-- @Witherable@. For example, if a type constructor @T@
-- already has instances of 'T.Traversable' and 'Filterable',
-- the next one line is sufficient to provide the @Witherable T@ instance.
--
-- > instance Witherable T

class (T.Traversable t, Filterable t) => Witherable t where

  -- | Effectful 'mapMaybe'.
  --
  -- @'wither' ('pure' . f) ≡ 'pure' . 'mapMaybe' f@
  -- 
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f
  {-# INLINE wither #-}

  -- | @Monadic variant of 'wither'. This may have more efficient implementation.@
  witherM :: Monad m => (a -> m (Maybe b)) -> t a -> m (t b)
  witherM = wither

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA f = wither $ \a -> (\b -> if b then Just a else Nothing) <$> f a

  witherMap :: (Applicative m) => (t b -> r) -> (a -> m (Maybe b)) -> t a -> m r
  witherMap p f = fmap p . wither f
  {-# INLINE witherMap #-}

  {-# MINIMAL #-}

instance Filterable Maybe where
  mapMaybe f = (>>= f)
  {-# INLINE mapMaybe #-}

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

#if !MIN_VERSION_base(4,16,0)

instance Filterable Option where
  mapMaybe f = (>>= Option . f)
  {-# INLINE mapMaybe #-}

instance Witherable Option where
  wither f (Option x) = Option <$> wither f x
  {-# INLINE wither #-}

-- Option doesn't have the necessary instances in Lens
--instance FilterableWithIndex () Option
--instance WitherableWithIndex () Option

#endif

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

instance Filterable ZipList where
  mapMaybe f = ZipList . Maybe.mapMaybe f . getZipList
  catMaybes = ZipList . Maybe.catMaybes . getZipList
  filter f = ZipList . Prelude.filter f . getZipList

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

instance Witherable ZipList where
  wither f = fmap ZipList . wither f . getZipList

instance Filterable IM.IntMap where
  mapMaybe = IM.mapMaybe
  filter = IM.filter

instance Witherable IM.IntMap where

instance Filterable (M.Map k) where
  mapMaybe = M.mapMaybe
  filter = M.filter

instance Witherable (M.Map k) where
#if MIN_VERSION_containers(0,5,8)
  wither f = M.traverseMaybeWithKey (const f)
#endif

instance (Eq k, Hashable k) => Filterable (HM.HashMap k) where
  mapMaybe = HM.mapMaybe
  filter = HM.filter

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where

instance Filterable Proxy where
 mapMaybe _ Proxy = Proxy

instance Witherable Proxy where
  wither _ Proxy = pure Proxy

instance Filterable (Const r) where
  mapMaybe _ (Const r) = Const r
  {-# INLINABLE mapMaybe #-}

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Filterable V.Vector where
  filter   = V.filter
  mapMaybe = V.mapMaybe

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

  witherM = V.mapMaybeM
  {-# INLINE witherM #-}

instance Filterable S.Seq where
  mapMaybe f = S.fromList . mapMaybe f . F.toList
  {-# INLINABLE mapMaybe #-}
  filter = S.filter

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

-- The instances for Compose, Product, and Sum are not entirely
-- unique. Any particular composition, product, or sum of functors
-- may support a variety of 'wither' implementations.

instance (Functor f, Filterable g) => Filterable (Compose f g) where
  mapMaybe f = Compose . fmap (mapMaybe f) . getCompose
  filter p = Compose . fmap (filter p) . getCompose
  catMaybes = Compose . fmap catMaybes . getCompose

instance (T.Traversable f, Witherable g) => Witherable (Compose f g) where
  wither f = fmap Compose . T.traverse (wither f) . getCompose
  witherM f = fmap Compose . T.mapM (witherM f) . getCompose
  filterA p = fmap Compose . T.traverse (filterA p) . getCompose

instance (Filterable f, Filterable g) => Filterable (P.Product f g) where
  mapMaybe f (P.Pair x y) = P.Pair (mapMaybe f x) (mapMaybe f y)
  filter p (P.Pair x y) = P.Pair (filter p x) (filter p y)
  catMaybes (P.Pair x y) = P.Pair (catMaybes x) (catMaybes y)

instance (Witherable f, Witherable g) => Witherable (P.Product f g) where
  wither f (P.Pair x y) = liftA2 P.Pair (wither f x) (wither f y)
  witherM f (P.Pair x y) = liftA2 P.Pair (witherM f x) (witherM f y)
  filterA p (P.Pair x y) = liftA2 P.Pair (filterA p x) (filterA p y)

instance (Filterable f, Filterable g) => Filterable (Sum.Sum f g) where
  mapMaybe f (Sum.InL x) = Sum.InL (mapMaybe f x)
  mapMaybe f (Sum.InR y) = Sum.InR (mapMaybe f y)

  catMaybes (Sum.InL x) = Sum.InL (catMaybes x)
  catMaybes (Sum.InR y) = Sum.InR (catMaybes y)

  filter p (Sum.InL x) = Sum.InL (filter p x)
  filter p (Sum.InR y) = Sum.InR (filter p y)

instance (Witherable f, Witherable g) => Witherable (Sum.Sum f g) where
  wither f (Sum.InL x) = Sum.InL <$> wither f x
  wither f (Sum.InR y) = Sum.InR <$> wither f y

  witherM f (Sum.InL x) = Sum.InL <$> witherM f x
  witherM f (Sum.InR y) = Sum.InR <$> witherM f y

  filterA f (Sum.InL x) = Sum.InL <$> filterA f x
  filterA f (Sum.InR y) = Sum.InR <$> filterA f y

deriving instance Filterable f => Filterable (IdentityT f)

instance Witherable f => Witherable (IdentityT f) where
  wither f (IdentityT m) = IdentityT <$> wither f m
  witherM f (IdentityT m) = IdentityT <$> witherM f m
  filterA p (IdentityT m) = IdentityT <$> filterA p m

instance Functor f => Filterable (MaybeT f) where
  mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

instance (T.Traversable t) => Witherable (MaybeT t) where
  wither f = fmap MaybeT . T.traverse (wither f) . runMaybeT
  witherM f = fmap MaybeT . T.mapM (wither f) . runMaybeT

deriving instance Filterable t => Filterable (Reverse t)

-- | Wither from right to left.
instance Witherable t => Witherable (Reverse t) where
  wither f (Reverse t) =
    fmap Reverse . forwards $ wither (coerce f) t
  -- We can't do anything special with witherM, because Backwards m is not
  -- generally a Monad.
  filterA f (Reverse t) =
    fmap Reverse . forwards $ filterA (coerce f) t

deriving instance Filterable t => Filterable (Backwards t)

instance Witherable t => Witherable (Backwards t) where
  wither f (Backwards xs) = Backwards <$> wither f xs
  witherM f (Backwards xs) = Backwards <$> witherM f xs
  filterA f (Backwards xs) = Backwards <$> filterA f xs

instance Filterable Generics.V1 where
  mapMaybe _ v = case v of {}
  catMaybes v = case v of {}
  filter _ v = case v of {}

instance Witherable Generics.V1 where
  wither _ v = pure $ case v of {}
  filterA _ v = pure $ case v of {}

instance Filterable Generics.U1 where
  mapMaybe _ _ = Generics.U1
  catMaybes _ = Generics.U1
  filter _ _ = Generics.U1

instance Witherable Generics.U1 where
  wither _ _ = pure Generics.U1
  filterA _ _ = pure Generics.U1

instance Filterable (Generics.K1 i c) where
  mapMaybe _ (Generics.K1 a) = Generics.K1 a
  catMaybes (Generics.K1 a) = Generics.K1 a
  filter _ (Generics.K1 a) = Generics.K1 a

instance Witherable (Generics.K1 i c) where
  wither _ (Generics.K1 a) = pure (Generics.K1 a)
  filterA _ (Generics.K1 a) = pure (Generics.K1 a)

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

-- | Indexed variant of 'Filterable'.
class (FunctorWithIndex i t, Filterable t) => FilterableWithIndex i t | t -> i where
  imapMaybe :: (i -> a -> Maybe b) -> t a -> t b
  imapMaybe f = catMaybes . imap f
  {-# INLINE imapMaybe #-}

  -- | @'ifilter' f . 'ifilter' g ≡ ifilter (\i -> 'liftA2' ('&&') (f i) (g i))@
  ifilter :: (i -> a -> Bool) -> t a -> t a
  ifilter f = imapMaybe $ \i a -> if f i a then Just a else Nothing
  {-# INLINE ifilter #-}

-- | Indexed variant of 'Witherable'.
class (TraversableWithIndex i t, Witherable t) => WitherableWithIndex i t | t -> i where
  -- | Effectful 'imapMaybe'.
  --
  -- @'iwither' (\ i -> 'pure' . f i) ≡ 'pure' . 'imapMaybe' f@
  iwither :: (Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
  iwither f = fmap catMaybes . itraverse f

  -- | @Monadic variant of 'wither'. This may have more efficient implementation.@
  iwitherM :: (Monad m) => (i -> a -> m (Maybe b)) -> t a -> m (t b)
  iwitherM = iwither

  ifilterA :: (Applicative f) => (i -> a -> f Bool) -> t a -> f (t a)
  ifilterA f = iwither (\i a -> (\b -> if b then Just a else Nothing) <$> f i a)

instance FilterableWithIndex () Maybe

instance WitherableWithIndex () Maybe

-- Option doesn't have the necessary instances in Lens
--instance FilterableWithIndex () Option
--instance WitherableWithIndex () Option

instance FilterableWithIndex Int []

instance FilterableWithIndex Int ZipList

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

instance (FunctorWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (i, j) (Compose f g) where
  imapMaybe f = Compose . imap (\i -> imapMaybe (\j -> f (i, j))) . getCompose
  ifilter p = Compose . imap (\i -> ifilter (\j -> p (i, j))) . getCompose

instance (TraversableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (i, j) (Compose f g) where
  iwither f = fmap Compose . itraverse (\i -> iwither (\j -> f (i, j))) . getCompose
  iwitherM f = fmap Compose . imapM (\i -> iwitherM (\j -> f (i, j))) . getCompose
  ifilterA p = fmap Compose . itraverse (\i -> ifilterA (\j -> p (i, j))) . getCompose

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
--
-- >>> ordNub [3,2,1,3,2,1]
-- [3,2,1]
--
ordNub :: (Witherable t, Ord a) => t a -> t a
ordNub = ordNubOn id
{-# INLINE ordNub #-}

-- | The 'ordNubOn' function behaves just like 'ordNub',
--   except it uses a another type to determine equivalence classes.
--
-- >>> ordNubOn fst [(True, 'x'), (False, 'y'), (True, 'z')]
-- [(True,'x'),(False,'y')]
--
ordNubOn :: (Witherable t, Ord b) => (a -> b) -> t a -> t a
ordNubOn p t = evalState (witherM f t) Set.empty where
    f a = state $ \s ->
#if MIN_VERSION_containers(0,6,3)
      -- insert in one go
      -- having if outside is important for performance,
      -- \x -> (if x ... , True)  -- is slower
      case Set.alterF (\x -> BoolPair x True) (p a) s of
        BoolPair True  s' -> (Nothing, s')
        BoolPair False s' -> (Just a,  s')
#else
      if Set.member (p a) s
      then (Nothing, s)
      else (Just a, Set.insert (p a) s)
#endif
{-# INLINE ordNubOn #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparison (like 'String').
--
-- >>> hashNub [3,2,1,3,2,1]
-- [3,2,1]
--
hashNub :: (Witherable t, Eq a, Hashable a) => t a -> t a
hashNub = hashNubOn id
{-# INLINE hashNub #-}

-- | The 'hashNubOn' function behaves just like 'ordNub',
--   except it uses a another type to determine equivalence classes.
--
-- >>> hashNubOn fst [(True, 'x'), (False, 'y'), (True, 'z')]
-- [(True,'x'),(False,'y')]
--
hashNubOn :: (Witherable t, Eq b, Hashable b) => (a -> b) -> t a -> t a
hashNubOn p t = evalState (witherM f t) HSet.empty
  where
    f a = state $ \s ->
      let g Nothing  = BoolPair False (Just ())
          g (Just _) = BoolPair True  (Just ())
      -- there is no HashSet.alterF, but toMap / fromMap are newtype wrappers.
      in case HM.alterF g (p a) (HSet.toMap s) of
        BoolPair True  s' -> (Nothing, HSet.fromMap s')
        BoolPair False s' -> (Just a,  HSet.fromMap s')
{-# INLINE hashNubOn #-}

-- used to implement *Nub functions.
data BoolPair a = BoolPair !Bool a deriving Functor

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
