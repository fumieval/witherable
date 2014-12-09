{-# LANGUAGE CPP #-}
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

-- | Like `traverse`, but you can remove elements instead of updating them.
-- @traverse f = wither (fmap Just . f)@
-- Minimal complete definition: `wither` or `catMaybes`.
class T.Traversable t => Witherable t where

  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f

  catMaybes :: Witherable t => t (Maybe a) -> t a
  catMaybes = runIdentity . wither pure

  witherM :: Monad m => (a -> MaybeT m b) -> t a -> m (t b)
  witherM f = unwrapMonad . wither (WrapMonad . runMaybeT . f)

-- | 'blightM' is 'witherM' with its arguments flipped.
blightM :: (Monad m, Witherable t) => t a -> (a -> MaybeT m b) -> m (t b)
blightM = flip witherM
{-# INLINE blightM #-}

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a

instance Witherable [] where
  wither f = fmap Maybe.catMaybes . T.traverse f

instance Witherable IM.IntMap where
  wither f = fmap IM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . IM.toList

instance Ord k => Witherable (M.Map k) where
  wither f = fmap M.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . M.toList

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where
  wither f = fmap HM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . HM.toList

#if !(MIN_VERSION_base(4,7,0))
instance F.Foldable (Const r) where
  foldMap _ _ = mempty

instance T.Traversable (Const r) where
  traverse _ (Const r) = pure (Const r)
#endif

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList