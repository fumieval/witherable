{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Witherable.Class
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Witherable.Class
  ( Filterable(..)
  , Witherable(..)
  )

where
import qualified Data.Maybe as Maybe
import Data.Bool (bool)
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
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
import Data.Proxy
import Data.Void
import Data.Coerce (coerce)
import qualified Prelude
import Prelude hiding (filter)

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
-- [/conservation/]
--   @'wither' ('fmap' 'Just' . f) ≡ 'traverse' f@
--
-- [/composition/]
--   @'Compose' . 'fmap' ('wither' f) . 'wither' g ≡ 'wither' ('Compose' . 'fmap' ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
-- Whenever @t@ is an //applicative transformation// in the sense described in the
-- 'Traversable' documentation,
--
--   @t . 'wither' f ≡ 'wither' (t . f)@
--
-- See the @Properties.md@ file in the git distribution for some special properties of
-- empty containers.

class (T.Traversable t, Filterable t) => Witherable t where

  -- | Effectful 'mapMaybe'.
  --
  -- @'wither' ('pure' . f) ≡ 'pure' . 'mapMaybe' f@
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f
  {-# INLINE wither #-}

  -- | @Monadic variant of 'wither'. This may have more efficient implementation.@
  witherM :: Monad m => (a -> m (Maybe b)) -> t a -> m (t b)
  witherM = wither

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA f = wither $ \a -> (\b -> if b then Just a else Nothing) <$> f a

  {-# MINIMAL #-}

-- | A default implementation for 'mapMaybe'.
mapMaybeDefault :: (F.Foldable f, Alternative f) => (a -> Maybe b) -> f a -> f b
mapMaybeDefault p = F.foldr (\x xs -> case p x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE mapMaybeDefault #-}

instance Filterable Maybe where
  mapMaybe f = (>>= f)
  {-# INLINE mapMaybe #-}

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

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
  mapMaybe = V.mapMaybe

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

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
