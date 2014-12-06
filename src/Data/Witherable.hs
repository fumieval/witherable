module Data.Witherable where
import qualified Data.Maybe as Maybe
import qualified Data.IntMap.Strict as StrictIM
import qualified Data.Map.Strict as StrictM
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Hashable
import Data.Functor.Identity

-- | Like `traverse`, but you can remove elements instead of updating them.
-- @traverse f = wither (fmap Just . f)@
-- Minimal complete definition: `wither` or `catMaybes`.
class T.Traversable t => Witherable t where

  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither = fmap catMaybes . traverse

  catMaybes :: Witherable t => t (Maybe a) -> t a
  catMaybes = runIdentity . wither pure

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a

instance Witherable [] where
  wither f = fmap Maybe.catMaybes . T.traverse f

instance Witherable StrictIM.IntMap where
  wither f = fmap StrictIM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . StrictIM.toList

instance Ord k => Witherable (StrictM.Map k) where
  wither f = fmap StrictM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . StrictM.toList

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where
  wither f = fmap HM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . HM.toList

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList