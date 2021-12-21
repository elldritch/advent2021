module Advent2021.Paths
  ( Path
  , aStar
  , reachable
  ) where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM, tailRecM2)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty as NESet
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafePartial)

reachable :: forall v. Ord v => (v -> List v) -> v -> Set v
reachable next start = reachableR Set.empty $ List.singleton start
  where
  reachableR :: Set v -> List v -> Set v
  reachableR seen queue = case List.uncons queue of
    Just { head, tail } ->
      let
        frontier = next head

        unseen = List.filter (not <<< flip Set.member seen) frontier

        seen' = seen <> Set.fromFoldable unseen

        queue' = unseen <> tail
      in
        reachableR seen' queue'
    Nothing -> seen

type Path v
  = NonEmptyList v

type History v e
  = { from :: v
    , distance :: e
    , estimate :: e
    }

aStar ::
  forall v e.
  Ord v =>
  Semiring e =>
  Ord e =>
  (v -> List { neighbor :: v, distance :: e }) ->
  (v -> e) ->
  v ->
  v ->
  Either String (Path v)
aStar next heuristic start destination = do
  completeFroms <-
    tailRecM2
      aStarR
      (Set.singleton start)
      (Map.singleton start { from: start, distance: zero, estimate: heuristic start })
  reconstructPath completeFroms
  where
  reconstructPath :: forall r. Map v { from :: v | r } -> Either String (Path v)
  reconstructPath completeFroms = tailRecM reconstructPathR (NEList.singleton destination)
    where
    reconstructPathR :: Path v -> Either String (Step (Path v) (Path v))
    reconstructPathR path =
      let
        curr = NEList.head path
      in
        if curr == start then
          pure $ Done $ NEList.reverse path
        else do
          { from } <- note "Impossible: reconstructing path with unknown vertex" $ Map.lookup curr completeFroms
          pure $ Loop $ NEList.cons from path

  -- This is a stack-safe variant of sortBy. See https://github.com/purescript/purescript-lists/issues/192.
  sortBy' :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> NonEmptyList a
  sortBy' cmp =
    unsafePartial fromJust
      <<< NEList.fromFoldable
      <<< Array.sortBy cmp
      <<< Array.fromFoldable

  aStarR ::
    Set v ->
    Map v (History v e) ->
    Either String (Step { a :: Set v, b :: Map v (History v e) } (Map v (History v e)))
  aStarR queue best = do
    neQueue <- note "Destination not reachable" $ NESet.fromSet queue
    let
      -- TODO: The biggest performance gains will probably be from using a real
      -- priority queue here. Doing this map lookup over and over again is slow.
      pQueue =
        map fst
          $ sortBy' (\(Tuple _ a) (Tuple _ b) -> comparingDistance a b)
          $ map (\position -> Tuple position (_.estimate <$> Map.lookup position best))
          $ NESet.toUnfoldable1 neQueue

      current = NEList.head pQueue
    if current == destination then
      pure $ Done best
    else do
      currentDistance <-
        map _.distance
          $ note "Impossible: current vertex has never been seen before"
          $ Map.lookup current best
      let
        frontier = next current

        { best', toExplore } = foldl (update { current, currentDistance }) { best': best, toExplore: Nil } frontier

        queue' = Set.fromFoldable $ (NEList.tail pQueue) <> toExplore
      pure $ Loop { a: queue', b: best' }
    where
    update ::
      { current :: v, currentDistance :: e } ->
      { best' :: Map v (History v e), toExplore :: List v } ->
      { neighbor :: v, distance :: e } ->
      { best' :: Map v (History v e), toExplore :: List v }
    update { current, currentDistance } acc@{ best', toExplore } { neighbor, distance: neighborDistance } = case Map.lookup neighbor best' of
      Just { distance } -> if distance' < distance then new else acc
      Nothing -> new
      where
      distance' = currentDistance + neighborDistance

      new =
        { best':
            Map.insert
              neighbor
              { from: current
              , distance: distance'
              , estimate: distance' + heuristic neighbor
              }
              best'
        , toExplore: neighbor : toExplore
        }

  comparingDistance :: Maybe e -> Maybe e -> Ordering
  comparingDistance (Just _) Nothing = LT

  comparingDistance Nothing (Just _) = GT

  comparingDistance Nothing Nothing = EQ

  comparingDistance (Just a) (Just b) = compare a b
