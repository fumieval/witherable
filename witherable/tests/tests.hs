{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Control.Arrow (first)
import Control.Monad ((<=<))
import Control.Monad.Trans.State (State, runState, state)
import Data.Hashable (Hashable)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.List (nub, nubBy)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.QuickCheck (Arbitrary (..), Fun, Property, applyFun, Function (..), functionMap, CoArbitrary, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import Witherable
import Prelude hiding (filter)

main :: IO ()
main = defaultMain $ testGroup "witherable"
  [ testGroup "Filterable"
    [ filterableLaws (Proxy @[])
    , filterableLaws (Proxy @Maybe)
    , filterableLaws (Proxy @(Either String))
    , filterableLaws (Proxy @V.Vector)
    , filterableLaws (Proxy @Seq.Seq)
    , filterableLaws (Proxy @IntMap.IntMap)
    , filterableLaws (Proxy @(Map.Map K))
    , filterableLaws (Proxy @(HashMap.HashMap K))
    , filterableLaws (Proxy @Wicked)
    ]

  , testGroup "Witherable"
    [ witherableLaws (Proxy @[])
    , witherableLaws (Proxy @Maybe)
    , witherableLaws (Proxy @(Either String))
    , witherableLaws (Proxy @V.Vector)
    , witherableLaws (Proxy @Seq.Seq)
#if MIN_VERSION_containers(0,6,3)
    -- traverse @IntMap is broken
    , witherableLaws (Proxy @IntMap.IntMap)
#endif
    , witherableLaws (Proxy @(Map.Map K))
    , witherableLaws (Proxy @(HashMap.HashMap K))
    -- Wicked is not Witherable, see https://github.com/fumieval/witherable/issues/63#issuecomment-834631975
    -- , witherableLaws (Proxy @Wicked)
    ]

  , nubProperties
  ]

-------------------------------------------------------------------------------
-- Filterable laws
-------------------------------------------------------------------------------

filterableLaws
  :: forall f.
     ( Filterable f, Typeable f
     , Arbitrary (f A), Show (f A), Eq (f A)
     , Arbitrary (f (Maybe A)), Show (f (Maybe A))
     , Show (f B), Eq (f B), Show (f C), Eq (f C)
     )
  => Proxy f
  -> TestTree
filterableLaws p = testGroup (show (typeRep p))
  [ testProperty "conservation" prop_conservation
  , testProperty "composition" prop_composition
  , testProperty "default filter" prop_default_filter
  , testProperty "default mapMaybe" prop_default_mapMaybe
  , testProperty "default catMaybes" prop_default_catMaybes
  ]
  where
    prop_conservation :: Fun A B -> f A -> Property
    prop_conservation f' xs =
        mapMaybe (Just . f) xs === fmap f xs
      where
        f = applyFun f'

    prop_composition :: Fun B (Maybe C) -> Fun A (Maybe B) -> f A -> Property
    prop_composition f' g' xs =
        mapMaybe f (mapMaybe g xs) === mapMaybe (f <=< g) xs
      where
        f = applyFun f'
        g = applyFun g'

    prop_default_filter :: Fun A Bool -> f A -> Property
    prop_default_filter f' xs =
        filter f xs === mapMaybe (\a -> if f a then Just a else Nothing) xs
      where
        f = applyFun f'

    prop_default_mapMaybe :: Fun A (Maybe B) -> f A -> Property
    prop_default_mapMaybe f' xs =
        mapMaybe f xs === catMaybes (fmap f xs)
      where
        f = applyFun f'

    prop_default_catMaybes :: f (Maybe A) -> Property
    prop_default_catMaybes xs = catMaybes xs === mapMaybe id xs

-------------------------------------------------------------------------------
-- Witherable laws
-------------------------------------------------------------------------------

witherableLaws
  :: forall f.
     ( Witherable f, Typeable f
     , Arbitrary (f A), Show (f A), Eq (f A)
     , Arbitrary (f (Maybe A)), Show (f (Maybe A))
     , Show (f B), Eq (f B), Show (f C), Eq (f C)
     )
  => Proxy f
  -> TestTree
witherableLaws p = testGroup (show (typeRep p))
  [ testProperty "default wither" prop_default_wither
  , testProperty "default witherM" prop_default_witherM
  , testProperty "default filterA" prop_default_filterA
  , testProperty "identity" prop_identity
  , testProperty "composition" prop_composition
  ]
  where
    prop_default_wither :: S -> Fun (A, S) (Maybe B, S) -> f A -> Property
    prop_default_wither s0 f' xs = equalState s0 xs
        (wither f)
        (fmap catMaybes . traverse f)
      where
        f :: A -> State S (Maybe B)
        f a = state $ \s -> applyFun f' (a, s)

    prop_default_witherM :: S -> Fun (A, S) (Maybe B, S) -> f A -> Property
    prop_default_witherM s0 f' xs = equalState s0 xs
        (witherM f)
        (wither f)
      where
        f a = state $ \s -> applyFun f' (a, s)

    prop_default_filterA :: S -> Fun (A, S) (Bool, S) -> f A -> Property
    prop_default_filterA s0 f' xs = equalState s0 xs
        (filterA f)
        (wither (\a -> (\b -> if b then Just a else Nothing) <$> f a))
      where
        f a = state $ \s -> applyFun f' (a, s)

    prop_identity :: S -> Fun (A, S) (B, S) -> f A -> Property
    prop_identity s0 f' xs = equalState s0 xs
        (wither (fmap Just . f))
        (traverse f)
      where
        f a = state $ \s -> applyFun f' (a, s)

    prop_composition :: S -> S -> Fun (B, S) (Maybe C, S) -> Fun (A, S) (Maybe B, S) -> f A -> Property
    prop_composition s0 s1 f' g' xs = equalStateC s0 s1 xs
         (Compose . fmap (wither f) . wither g)
         (wither (Compose . fmap (wither f) . g))
      where
        f a = state $ \s -> applyFun f' (a, s)
        g b = state $ \s -> applyFun g' (b, s)

    equalState
        :: (Eq b, Show b)
        => S -> a -> (a -> State S b) -> (a -> State S b) -> Property
    equalState s0 xs f g = runState (f xs) s0 === runState (g xs) s0

    equalStateC
        :: forall a b. (Eq b, Show b)
        => S -> S -> a -> (a -> Compose (State S) (State S) b) -> (a -> Compose (State S) (State S) b) -> Property
    equalStateC s0 s1 xs f g = run (f xs) === run (g xs)
      where
        run :: Compose (State S) (State S) b -> ((b, S), S)
        run m = first (\x -> runState x s1) (runState (getCompose m) s0)

-------------------------------------------------------------------------------
-- Nub "laws"
-------------------------------------------------------------------------------

nubProperties :: TestTree
nubProperties = testGroup "nub"
  [ testProperty "ordNub" prop_ordNub
  , testProperty "ordNubOn" prop_ordNubOn
  , testProperty "hashNub" prop_hashNub
  , testProperty "hashNubOn" prop_hashNubOn
  , testProperty "ordNub is lazy" prop_lazy_ordNub
  , testProperty "hashNub is lazy" prop_lazy_hashNub
  ]
  where
    prop_ordNub :: [A] -> Property
    prop_ordNub xs = nub xs === ordNub xs

    prop_hashNub :: [A] -> Property
    prop_hashNub xs = nub xs === hashNub xs

    prop_ordNubOn :: Fun A B -> [A] -> Property
    prop_ordNubOn f' xs = nubBy ((==) `on` f) xs === ordNubOn f xs
      where
        f = applyFun f'

    prop_hashNubOn :: Fun A B -> [A] -> Property
    prop_hashNubOn f' xs = nubBy ((==) `on` f) xs === hashNubOn f xs
      where
        f = applyFun f'

    prop_lazy_ordNub :: Property
    prop_lazy_ordNub = take 3 (ordNub ('x' : 'y' : 'z' : 'z' : error "bottom")) === "xyz"

    prop_lazy_hashNub :: Property
    prop_lazy_hashNub = take 3 (hashNub ('x' : 'y' : 'z' : 'z' : error "bottom")) === "xyz"

-------------------------------------------------------------------------------
-- "Poly"
-------------------------------------------------------------------------------

newtype A = A Int
  deriving (Eq, Ord, Show, Hashable, Arbitrary, CoArbitrary)

instance Function A where
  function = functionMap coerce A

newtype B = B Int
  deriving (Eq, Ord, Show, Hashable, Arbitrary, CoArbitrary)

instance Function B where
  function = functionMap coerce B

newtype C = C Int
  deriving (Eq, Ord, Show, Hashable, Arbitrary, CoArbitrary)

instance Function C where
  function = functionMap coerce C

newtype K = K Int
  deriving (Eq, Ord, Show, Hashable, Arbitrary, CoArbitrary)

instance Function K where
  function = functionMap coerce K

newtype S = S Int
  deriving (Eq, Ord, Show, Hashable, Arbitrary, CoArbitrary)

instance Function S where
  function = functionMap coerce S

-------------------------------------------------------------------------------
-- Wicked
-------------------------------------------------------------------------------

newtype Wicked a = W [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Filterable Wicked where
  -- mapMaybe f (W [a1,a2,...]) = W [b1, b2, ...]
  -- if all of [f a1, f a2, ...] are Just. Otherwise, it returns (W []).
  mapMaybe f = fromMaybe (W []) . traverse f

-- default implementation in terms of Filterable
instance Witherable Wicked

instance Arbitrary a => Arbitrary (Wicked a) where
    arbitrary = W <$> arbitrary
    shrink (W xs) = map W (shrink xs)
