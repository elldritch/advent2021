module Advent2021.Puzzles.D11
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Grid (Grid, around, gridP, insertWith)
import Advent2021.Grid as Grid
import Advent2021.Helpers (fix)
import Advent2021.Parsers (runParser)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (concat)
import Data.List as List
import Data.List.Lazy (iterate)
import Data.List.Lazy as ListL
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof)

type Octopus
  = Int

type Octopuses
  = Grid Octopus

octopusesP :: Parser Octopuses
octopusesP = gridP identity <* eof

step :: Octopuses -> Octopuses
step prev = fix flash incremented
  where
  incremented :: Octopuses
  incremented = map (_ + 1) prev

  flash :: Octopuses -> Octopuses
  flash octopuses = flashed
    where
    willFlash = map fst $ List.filter (\(Tuple _ e) -> e > 9) $ Grid.toUnfoldable octopuses

    willFlash' = Set.fromFoldable willFlash

    aroundFlash = concat $ around octopuses <$> willFlash

    energized =
      foldl
        (\grid pos -> insertWith (\old new -> if old == 0 then 0 else old + new) pos 1 grid)
        octopuses
        $ fst
        <$> aroundFlash

    flashed = mapWithIndex (\pos v -> if Set.member pos willFlash' then 0 else v) energized

part1 :: String -> Either String Int
part1 input = do
  octopuses <- runParser octopusesP input
  let
    steps = iterate step octopuses

    nSteps = ListL.take (100 + 1) steps
  pure $ foldl (\count grid -> count + countFlashes grid) 0 nSteps
  where
  countFlashes :: Octopuses -> Int
  countFlashes = foldl (\count energy -> count + if energy == 0 then 1 else 0) 0

part2 :: String -> Either String Int
part2 input = pure 0
