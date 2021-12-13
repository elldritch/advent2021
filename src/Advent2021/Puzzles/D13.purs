module Advent2021.Puzzles.D13
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser)
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Show.Generic (genericShow)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof, string)
import Text.Parsing.StringParser.Combinators (sepEndBy, sepEndBy1)

type Dot
  = { x :: Int, y :: Int }

dotP :: Parser Dot
dotP = do
  x <- integer
  _ <- char ','
  y <- integer
  pure { x, y }

type Paper
  = List Dot

data FoldInstruction
  = AlongX Int
  | AlongY Int

derive instance genericFold :: Generic FoldInstruction _

instance showFold :: Show FoldInstruction where
  show = genericShow

foldInstructionP :: Parser FoldInstruction
foldInstructionP = do
  _ <- string "fold along "
  axis <- (AlongX <$ char 'x') <|> (AlongY <$ char 'y')
  _ <- char '='
  coordinate <- integer
  pure $ axis coordinate

inputP :: Parser { dots :: Paper, foldInstructions :: NonEmptyList FoldInstruction }
inputP = do
  dots <- sepEndBy dotP newline
  newline
  foldInstructions <- sepEndBy1 foldInstructionP newline
  eof
  pure { dots, foldInstructions }

foldPaper :: Paper -> FoldInstruction -> Paper
foldPaper dots instruction =
  List.nub
    $ case instruction of
        AlongX line -> map (\{ x, y } -> if x > line then { x: line - (x - line), y } else { x, y }) dots
        AlongY line -> map (\{ x, y } -> if y > line then { x, y: line - (y - line) } else { x, y }) dots

part1 :: String -> Either String Int
part1 input = do
  { dots, foldInstructions } <- runParser inputP input
  let
    folded = foldPaper dots $ NEList.head foldInstructions
  pure $ List.length folded

part2 :: String -> Either String Int
part2 input = pure 0
