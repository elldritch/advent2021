module Advent2021.Grid
  ( Grid
  , Position
  , adjacent
  , gridP
  ) where

import Prelude
import Advent2021.Parsers (digit, newline)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), concat, mapMaybe, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (many, sepEndBy)

type Position
  = { x :: Int, y :: Int }

type Grid t
  = Map Position t

gridP :: forall t. (Int -> t) -> Parser (Grid t)
gridP fromDigit = do
  cells <- sepEndBy (many $ fromDigit <$> digit) newline <* eof
  pure $ Map.fromFoldable $ addPositions $ cells
  where
  addPositions :: List (List t) -> List (Tuple Position t)
  addPositions heights =
    concat
      $ mapWithIndex
          ( \y row ->
              mapWithIndex
                ( \x height ->
                    Tuple { x, y } height
                )
                row
          )
          heights

adjacent :: forall t. Grid t -> Position -> List (Tuple Position t)
adjacent grid { x, y } = do
  delta <- -1 : 1 : Nil
  mapMaybe (\p -> Tuple p <$> lookup' p) $ { x: x + delta, y } : { x, y: y + delta } : Nil
  where
  lookup' p = Map.lookup p grid
