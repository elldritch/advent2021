module Advent2021.Puzzles.D20
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Debug (undefined)
import Advent2021.Grid (Grid)
import Control.Alt ((<|>))
import Data.Either (Either)
import Data.List.NonEmpty (NonEmptyList)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char)

data Pixel
  = Dark
  | Light

pixelP :: Parser Pixel
pixelP = (char '#' $> Light) <|> (char '.' $> Dark)

inputP :: Parser { algorithm :: NonEmptyList Pixel, image :: Grid Pixel }
inputP = undefined

part1 :: String -> Either String Int
part1 input = pure 0

part2 :: String -> Either String Int
part2 input = pure 0
