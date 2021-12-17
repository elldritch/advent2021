module Advent2021.Grid
  ( Grid
  , Position
  , adjacent
  , around
  , dimensions
  , fromFoldable1
  , gridP
  , lookup
  , showGrid
  , showGrid'
  , toUnfoldable
  , toUnfoldable1
  , update
  ) where

import Prelude
import Advent2021.Parsers (digit, newline)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List (List(..), groupBy, range, sortBy, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup.Foldable (class Foldable1, maximumBy)
import Data.String as String
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1)
import Partial.Unsafe (unsafePartial)
import PointFree ((<..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy1)

type Position
  = { x :: Int, y :: Int }

-- Grids are finite, rectangular grids.
--
-- They are always rooted at the top-left at (0, 0), such that X coordinates
-- increase to the right and Y coordinates increase downwards.
--
-- Negative X or Y coordinates are not supported.
--
-- Grids are represented as dense Maps of every Position in the current grid to
-- its value.
newtype Grid t
  = Grid (Map Position t)

derive newtype instance eqGrid :: Eq t => Eq (Grid t)

derive newtype instance ordGrid :: Ord t => Ord (Grid t)

derive newtype instance showGridInstance :: (Show t) => Show (Grid t)

derive newtype instance functorGrid :: Functor Grid

derive newtype instance functorWithIndexGrid :: FunctorWithIndex Position Grid

derive newtype instance foldableGrid :: Foldable Grid

derive newtype instance foldableWithIndexGrid :: FoldableWithIndex Position Grid

derive newtype instance traversableGrid :: Traversable Grid

derive newtype instance traversableWithIndexGrid :: TraversableWithIndex Position Grid

unGrid :: forall t. Grid t -> Map Position t
unGrid (Grid m) = m

gridP :: forall t. (Int -> t) -> Parser (Grid t)
gridP fromDigit = do
  cells <- sepEndBy1 (many1 $ fromDigit <$> digit) newline
  pure $ Grid $ Map.fromFoldable $ addPositions $ cells
  where
  addPositions :: NonEmptyList (NonEmptyList t) -> NonEmptyList (Tuple Position t)
  addPositions heights =
    NEList.concat
      $ mapWithIndex
          ( \y row ->
              mapWithIndex
                ( \x height ->
                    Tuple { x, y } height
                )
                row
          )
          heights

showGrid :: forall t. Show t => Grid t -> String
showGrid = showGrid' show

showGrid' :: forall t. (t -> String) -> Grid t -> String
showGrid' show' grid =
  String.joinWith "\n"
    $ Array.fromFoldable
    $ map (String.joinWith "" <<< Array.fromFoldable <<< (map \(Tuple _ v) -> show' v))
    $ groupBy ((_ == EQ) <.. comparing (_.y <<< fst))
    $ sortBy (comparing $ _.y <<< fst)
    $ sortBy (comparing $ _.x <<< fst)
    $ toUnfoldable grid

toUnfoldable1 :: forall t f. Unfoldable1 f => Grid t -> f (Tuple Position t)
toUnfoldable1 =
  NEArray.toUnfoldable1
    <<< unsafePartial fromJust
    <<< NEArray.fromArray
    <<< toUnfoldable

toUnfoldable :: forall t f. Unfoldable f => Grid t -> f (Tuple Position t)
toUnfoldable = Map.toUnfoldable <<< unGrid

fromFoldable1 :: forall t f. Foldable1 f => t -> f (Tuple Position t) -> Grid t
fromFoldable1 default elements = populated
  where
  maxX = _.x $ fst $ maximumBy (comparing $ _.x <<< fst) elements

  maxY = _.y $ fst $ maximumBy (comparing $ _.y <<< fst) elements

  positions = do
    x <- range 0 maxX
    y <- range 0 maxY
    pure { x, y }

  gridOfDefaults = Grid $ Map.fromFoldable $ map (_ `Tuple` default) positions

  populated = foldl (\grid (Tuple p e) -> update (const e) p grid) gridOfDefaults elements

-- TODO: Add a "resize" operation?
update :: forall t. (t -> t) -> Position -> Grid t -> Grid t
update f k (Grid m) = Grid $ Map.update (Just <<< f) k m

lookup :: forall t. Grid t -> Position -> Maybe t
lookup (Grid m) k = Map.lookup k m

dimensions :: forall t. Grid t -> { x :: Int, y :: Int }
dimensions (Grid m) =
  (\{ x, y } -> { x: x + 1, y: y + 1 })
    $ foldlWithIndex
        (\{ x, y } { x: maxX, y: maxY } _ -> { x: max maxX x, y: max maxY y })
        { x: 0, y: 0 }
        m

adjacent :: forall t. Grid t -> Position -> List (Tuple Position t)
adjacent (Grid grid) { x, y } = do
  delta <- -1 : 1 : Nil
  List.mapMaybe (\p -> Tuple p <$> lookup' p) $ { x: x + delta, y } : { x, y: y + delta } : Nil
  where
  lookup' p = Map.lookup p grid

around :: forall t. Grid t -> Position -> List (Tuple Position t)
around (Grid grid) { x, y } =
  List.filter (\(Tuple { x: x', y: y' } _) -> not (x' == x && y' == y))
    $ List.catMaybes
    $ do
        dx <- delta
        dy <- delta
        let
          position = { x: x + dx, y: y + dy }
        pure $ Tuple position <$> Map.lookup position grid
  where
  delta = range (-1) 1
