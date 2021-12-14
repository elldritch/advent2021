module Advent2021.Helpers
  ( uniqueCounts
  , fix
  , iterateN
  ) where

import Prelude
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.State (State, evalState, get, put)
import Data.Foldable (class Foldable, foldl)
import Data.List.Lazy (iterate)
import Data.List.Lazy as ListL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

iterateN :: forall a. (a -> a) -> a -> Int -> a
iterateN f initial n = unsafePartial $ fromJust $ ListL.head $ ListL.drop n $ iterate f initial

fix :: forall a. Eq a => (a -> a) -> a -> a
fix f initial = evalState (untilJust $ f') initial
  where
  f' :: State a (Maybe a)
  f' = do
    last <- get
    let
      next = f last
    if next == last then
      pure $ Just next
    else do
      put next
      pure Nothing

uniqueCounts :: forall f a i. Foldable f => Ord a => Semiring i => f a -> Map a i
uniqueCounts = foldl (\m e -> Map.insertWith add e one m) Map.empty
