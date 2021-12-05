module Advent2021.Puzzles.D4
  ( part1
  ) where

import Prelude

import Advent2021.Debug (undefined)
import Advent2021.Parsers (integer, newline, runParser, space)
import Data.Either (Either)
import Data.List.Types (List, NonEmptyList)
import Data.Unfoldable (replicateA)
import Debug (spy)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy, sepBy1)

type Board
  = Array (Array Int)

boardP :: Parser Board
boardP = replicateA 5 (rowP <* newline)
  where
  rowP :: Parser (Array Int)
  rowP = replicateA 5 (space *> integer)

rows :: Board -> Array Int
rows = undefined

cols :: Board -> Array Int
cols = undefined

part1 :: String -> Either String Int
part1 input = do
  { draws, boards } <- spy "input" $ runParser inputP input

  -- Fold boards across draw order until first win
  -- Alternative: map boards across draw order, and pick the first winning one
  pure 0
  where
  drawsP :: Parser (NonEmptyList Int)
  drawsP = (sepBy1 integer (char ',')) <* newline

  inputP :: Parser { draws :: NonEmptyList Int, boards :: List Board }
  inputP = do
    draws <- drawsP
    newline
    boards <- sepBy boardP newline
    eof
    pure $ { draws, boards }
