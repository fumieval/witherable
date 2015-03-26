{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, UndecidableInstances, FlexibleContexts #-}
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
module Data.Witherable where
import qualified Data.Maybe as Maybe
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Hashable
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Data.Monoid
#if (MIN_VERSION_base(4,7,0))
import Data.Proxy
#endif

-- | Like `traverse`, but you can remove elements instead of updating them.
--
-- @'traverse' f ≡ 'wither' ('fmap' 'Just' . f)@
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/identity/]
--   @'wither' ('pure' . Just) ≡ 'pure'@
--
-- [/composition/]
--   @Compose . fmap ('wither' f) . 'wither' g ≡ 'wither' (Compose . fmap ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
--   @t . 'wither' f = 'wither' (t . f)@
--
-- Minimal complete definition: `wither` or `mapMaybe` or `catMaybes`.
-- The default definitions can be overridden for efficiency.

class T.Traversable t => Witherable t where

  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f

  mapMaybe :: (a -> Maybe b) -> t a -> t b
  mapMaybe f = runIdentity . wither (Identity . f)
  {-# INLINE mapMaybe #-}

  catMaybes :: t (Maybe a) -> t a
  catMaybes = mapMaybe id
  {-# INLINE catMaybes #-}

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA f = wither (\a -> (\b -> if b then Just a else Nothing) <$> f a)

  filter :: (a -> Bool) -> t a -> t a
  filter f = runIdentity . filterA (Identity . f)
  {-# INLINE filter #-}

witherM :: (Witherable t, Monad m) => (a -> MaybeT m b) -> t a -> m (t b)
witherM f = unwrapMonad . wither (WrapMonad . runMaybeT . f)
{-# INLINE witherM #-}

-- | 'blightM' is 'witherM' with its arguments flipped.
blightM :: (Monad m, Witherable t) => t a -> (a -> MaybeT m b) -> m (t b)
blightM = flip witherM
{-# INLINE blightM #-}

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

instance Monoid e => Witherable (Either e) where
  wither _ (Left e) = pure (Left e)
  wither f (Right a) = fmap (maybe (Left mempty) Right) (f a)
  {-# INLINABLE wither #-}

instance Witherable [] where
  wither f = go where
    go (x:xs) = maybe id (:) <$> f x <*> go xs
    go [] = pure []
  {-# INLINE wither #-}
  mapMaybe = Maybe.mapMaybe
  {-# INLINE mapMaybe #-}
  catMaybes = Maybe.catMaybes
  {-# INLINE catMaybes #-}
  filter = Prelude.filter
  {-# INLINE filter #-}

instance Witherable IM.IntMap where
  mapMaybe = IM.mapMaybe
  {-# INLINE mapMaybe #-}
  filter = IM.filter
  {-# INLINE filter #-}

instance Ord k => Witherable (M.Map k) where
  mapMaybe = M.mapMaybe
  {-# INLINE mapMaybe #-}
  filter = M.filter
  {-# INLINE filter #-}

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where
  wither f = fmap HM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . HM.toList
  {-# INLINABLE wither #-}
  filter = HM.filter
  {-# INLINE filter #-}

#if (MIN_VERSION_base(4,7,0))
instance Witherable Proxy where
  wither _ Proxy = pure Proxy
#endif

#if !(MIN_VERSION_base(4,7,0))
instance F.Foldable (Const r) where
  foldMap _ _ = mempty

instance T.Traversable (Const r) where
  traverse _ (Const r) = pure (Const r)

instance F.Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right a) = f a

instance T.Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y
#endif

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}
  filter = V.filter
  {-# INLINE filter #-}

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}
  filter = S.filter
  {-# INLINE filter #-}

-- | Traversable containers which hold 'Maybe' are witherable.
newtype Chipped t a = Chipped { getChipped :: t (Maybe a) } deriving (Functor, F.Foldable, T.Traversable)

deriving instance Show (t (Maybe a)) => Show (Chipped t a)
deriving instance Read (t (Maybe a)) => Read (Chipped t a)
deriving instance Eq (t (Maybe a)) => Eq (Chipped t a)
deriving instance Ord (t (Maybe a)) => Ord (Chipped t a)

instance Applicative t => Applicative (Chipped t) where
  pure a = Chipped (pure (pure a))
  Chipped f <*> Chipped t = Chipped (liftA2 (<*>) f t)

instance T.Traversable t => Witherable (Chipped t) where
  wither f = fmap Chipped . T.traverse (wither f) . getChipped
