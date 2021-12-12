module Advent2021.Helpers
  ( fix
  , iterateN
  ) where

import Prelude
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.State (State, evalState, get, put)
import Data.List.Lazy (iterate)
import Data.List.Lazy as ListL
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
