module Advent2021.Helpers
  ( fix
  , fixM
  , fixM'
  , iterateN
  , iterateNM
  , uniqueCounts
  ) where

import Prelude
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Monad.State.Trans (evalStateT, StateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (class Foldable, foldl)
import Data.Identity (Identity(..))
import Data.List.Lazy (iterate)
import Data.List.Lazy as ListL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import PointFree ((<..))

iterateN :: forall a. (a -> a) -> a -> Int -> a
iterateN f initial n = unsafePartial $ fromJust $ ListL.head $ ListL.drop n $ iterate f initial

iterateNM :: forall m a. Monad m => (a -> m a) -> a -> Int -> m a
iterateNM _ initial 0 = pure initial

iterateNM f initial n = iterateNM f initial (n - 1) >>= f

fix :: forall a. Eq a => (a -> a) -> a -> a
fix f initial = x
  where
  (Identity x) = fixM (pure <<< f) initial

fixM :: forall m a. MonadRec m => Eq a => (a -> m a) -> a -> m a
fixM = map _.result <.. fixM'

fixM' :: forall m a. MonadRec m => Eq a => (a -> m a) -> a -> m { result :: a, iterations :: Int }
fixM' f initial = evalStateT (untilJust f') { result: initial, iterations: 0 }
  where
  f' :: StateT { result :: a, iterations :: Int } m (Maybe { result :: a, iterations :: Int })
  f' = do
    { result: last, iterations } <- get
    next <- lift $ f last
    if next == last then
      pure $ Just { result: next, iterations }
    else do
      put { result: next, iterations: iterations + 1 }
      pure Nothing

uniqueCounts :: forall f a i. Foldable f => Ord a => Semiring i => f a -> Map a i
uniqueCounts = foldl (\m e -> Map.insertWith add e one m) Map.empty
