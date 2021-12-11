module Advent2021.Puzzles.D4
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (integer, newline, runParser, space)
import Data.Either (Either, note)
import Data.Foldable (any, find, foldl, or, sum)
import Data.List (concat, head, null, transpose, (:))
import Data.List as List
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Unfoldable (replicateA)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (sepBy)

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
  lineWon line = null $ List.difference line draws

type Result
  = { winner :: Board, drawn :: Draws }

run :: forall r. (List Board -> Draws -> { result :: Maybe Result | r }) -> String -> Either String Int
run getWinner input = do
  { draws, boards } <- runParser inputP input
  let
    { result } = getWinner boards draws
  { winner, drawn } <- note "Invalid input: no winning board" result
  computeScore winner drawn
  where
  drawsP :: Parser Draws
  drawsP = (sepBy integer (char ',')) <* newline

  inputP :: Parser { draws :: Draws, boards :: List Board }
  inputP = do
    draws <- drawsP
    newline
    boards <- sepBy boardP newline
    eof
    pure $ { draws, boards }

  computeScore :: Board -> Draws -> Either String Int
  computeScore board draws = do
    winningDraw <- note "Impossible: no numbers were drawn" $ head draws
    pure $ winningDraw * sum (List.difference (concat board) draws)

part1 :: String -> Either String Int
part1 = run $ \boards -> foldl (findWinner boards) { drawn: mempty, result: Nothing }
  where
  findWinner ::
    List Board ->
    { drawn :: Draws, result :: Maybe Result } ->
    Int ->
    { drawn :: Draws, result :: Maybe Result }
  findWinner boards { drawn, result } draw = case result of
    Just _ -> { drawn, result }
    Nothing ->
      { drawn: drawn'
      , result: find (flip won drawn') boards >>= pure <$> { winner: _, drawn: drawn' }
      }
      where
      drawn' = draw : drawn

part2 :: String -> Either String Int
part2 =
  run \boards ->
    foldl findWinner
      { drawn: mempty
      , result: Nothing
      , remaining: Set.fromFoldable boards
      }
  where
  findWinner ::
    { drawn :: Draws, remaining :: Set Board, result :: Maybe Result } ->
    Int ->
    { drawn :: Draws, remaining :: Set Board, result :: Maybe Result }
  findWinner { drawn, result, remaining } draw = case head $ Set.toUnfoldable winners of
    Just w ->
      { drawn: drawn'
      , remaining: Set.difference remaining winners
      , result: Just { winner: w, drawn: drawn' }
      }
    Nothing -> { drawn: drawn', remaining, result }
    where
    drawn' = draw : drawn

    winners = Set.filter (flip won drawn') remaining
