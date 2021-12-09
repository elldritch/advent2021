module Advent2021.Puzzles.D4
  ( part1
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser, space)
import Data.Either (Either, note)
import Data.Foldable (any, find, foldl, or, sum)
import Data.List (concat, difference, head, null, transpose, (:))
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy, sepBy1)

type Board
  = List (List Int)

boardP :: Parser Board
boardP = replicateA 5 (rowP <* newline)
  where
  rowP :: Parser (List Int)
  rowP = replicateA 5 (space *> integer)

type Line
  = List Int

rows :: Board -> List Line
rows = identity

cols :: Board -> List Line
cols = transpose

type Draws
  = List Int

won :: Board -> Draws -> Boolean
won board draws = or $ map (\f -> any lineWon (f board)) [ rows, cols ]
  where
  lineWon :: Line -> Boolean
  lineWon line = null $ difference line draws

part1 :: String -> Either String Int
part1 input = do
  { draws, boards } <- runParser inputP input
  let
    { drawn, winner } = foldl (findWinner boards) { drawn: mempty, winner: Nothing } draws
  winnerBoard <- note "Invalid input: no winning board" winner
  computeScore winnerBoard drawn
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

  findWinner ::
    List Board ->
    { drawn :: List Int, winner :: Maybe Board } ->
    Int ->
    { drawn :: List Int, winner :: Maybe Board }
  findWinner boards { drawn, winner } draw = case winner of
    Just _ -> { drawn, winner }
    Nothing ->
      let
        drawn' = draw : drawn
      in
        { drawn: drawn'
        , winner: find (flip won drawn') boards
        }

  computeScore :: Board -> Draws -> Either String Int
  computeScore board draws = do
    winningDraw <- note "Impossible: no numbers were drawn" $ head draws
    pure $ winningDraw * sum (difference (concat board) draws)
