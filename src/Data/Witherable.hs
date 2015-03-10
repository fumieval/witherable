{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Witherable
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module generalizes filterable containers.
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
-- @traverse f ≡ wither (fmap Just . f)@
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/identity/]
--   @wither (pure . Just) ≡ pure@
--
-- [/composition/]
--   @Compose . fmap (wither f) . wither g ≡ wither (Compose . fmap (maybe (pure Nothing) f) . g)@
--
-- Parametricity implies the naturality law:
--
-- @t . wither f = wither (t . f)@
--
-- Minimal complete definition: `wither` or `catMaybes`.
-- The default definitions can be overriden for efficiency.
class T.Traversable t => Witherable t where

  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f

  catMaybes :: t (Maybe a) -> t a
  catMaybes = runIdentity . wither pure

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA f = wither (\a -> (\b -> if b then Just a else Nothing) <$> f a)

  filter :: (a -> Bool) -> t a -> t a
  filter f = runIdentity . filterA (Identity . f)

witherM :: (Witherable t, Monad m) => (a -> MaybeT m b) -> t a -> m (t b)
witherM f = unwrapMonad . wither (WrapMonad . runMaybeT . f)

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
  wither f = fmap Maybe.catMaybes . T.traverse f
  {-# INLINABLE wither #-}
  catMaybes = Maybe.catMaybes
  {-# INLINABLE catMaybes #-}
  filter = Prelude.filter
  {-# INLINABLE filter #-}

instance Witherable IM.IntMap where
  wither f = fmap IM.fromAscList . wither (\(i, a) -> fmap ((,) i) <$> f a) . IM.toList
  {-# INLINABLE wither #-}
  filter = IM.filter
  {-# INLINABLE filter #-}

instance Ord k => Witherable (M.Map k) where
  wither f = fmap M.fromAscList . wither (\(i, a) -> fmap ((,) i) <$> f a) . M.toList
  {-# INLINABLE wither #-}
  filter = M.filter
  {-# INLINABLE filter #-}

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where
  wither f = fmap HM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . HM.toList
  {-# INLINABLE wither #-}
  filter = HM.filter
  {-# INLINABLE filter #-}

#if (MIN_VERSION_base(4,7,0))
instance Witherable Proxy where
  wither _ Proxy = pure Proxy
#endif

#if !(MIN_VERSION_base(4,7,0))
instance F.Foldable (Const r) where
  foldMap _ _ = mempty

instance T.Traversable (Const r) where
  traverse _ (Const r) = pure (Const r)

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y
#endif

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}
