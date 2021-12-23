module Advent2021.Helpers
  ( fix
  , fixM
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
fixM f initial = evalStateT (untilJust f') initial
  where
  f' :: StateT a m (Maybe a)
  f' = do
    last <- get
    next <- lift $ f last
    if next == last then
      pure $ Just next
    else do
      put next
      pure Nothing

uniqueCounts :: forall f a i. Foldable f => Ord a => Semiring i => f a -> Map a i
uniqueCounts = foldl (\m e -> Map.insertWith add e one m) Map.empty
