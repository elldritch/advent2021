module Main
  ( main
  ) where

import Prelude
import Advent2021.Puzzles.D1 as D1
import Advent2021.Puzzles.D2 as D2
import Advent2021.Puzzles.D3 as D3
import Advent2021.Puzzles.D4 as D4
import Advent2021.Puzzles.D5 as D5
import Advent2021.Puzzles.D6 as D6
import Advent2021.Puzzles.D7 as D7
import Advent2021.Puzzles.D8 as D8
import Advent2021.Puzzles.D9 as D9
import Advent2021.Puzzles.D10 as D10
import Advent2021.Puzzles.D11 as D11
import Advent2021.Puzzles.D12 as D12
import Advent2021.Puzzles.D13 as D13
import Advent2021.Puzzles.D14 as D14
import Advent2021.Puzzles.D15 as D15
import Advent2021.Puzzles.D16 as D16
import Advent2021.Puzzles.D17 as D17
import Advent2021.Puzzles.D18 as D18
import Advent2021.Puzzles.D19 as D19
import Advent2021.Puzzles.D20 as D20
import Advent2021.Puzzles.D21 as D21
import Advent2021.Puzzles.D22 as D22
import Advent2021.Puzzles.D23 as D23
import Advent2021.Puzzles.D24 as D24
import Advent2021.Puzzles.D25 as D25
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log, warn)
import Effect.Exception (catchException, message, name, stack, throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (exit)
import Options.Applicative (header, (<**>))
import Options.Applicative.Builder (fullDesc, info, int, long, option, strOption)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types (Parser, ParserInfo)

data Options
  = Options
    { day :: Int
    , part :: Int
    , inputFile :: FilePath
    }

optsParser :: Parser Options
optsParser = ado
  day <- option int (long "day")
  part <- option int (long "part")
  inputFile <- strOption (long "input")
  in Options { day, part, inputFile }

argparse :: ParserInfo Options
argparse = info (optsParser <**> helper) (fullDesc <> header "Advent of Code 2021 solver")

main :: Effect Unit
main = do
  Options { day, part, inputFile } <- execParser argparse
  case { day, part } of
    { day: 1, part: 1 } -> run inputFile D1.part1
    { day: 1, part: 2 } -> run inputFile D1.part2
    { day: 2, part: 1 } -> run inputFile D2.part1
    { day: 2, part: 2 } -> run inputFile D2.part2
    { day: 3, part: 1 } -> run inputFile D3.part1
    { day: 3, part: 2 } -> run inputFile D3.part2
    { day: 4, part: 1 } -> run inputFile D4.part1
    { day: 4, part: 2 } -> run inputFile D4.part2
    { day: 5, part: 1 } -> run inputFile D5.part1
    { day: 5, part: 2 } -> run inputFile D5.part2
    { day: 6, part: 1 } -> run inputFile D6.part1
    { day: 6, part: 2 } -> run inputFile D6.part2
    { day: 7, part: 1 } -> run inputFile D7.part1
    { day: 7, part: 2 } -> run inputFile D7.part2
    { day: 8, part: 1 } -> run inputFile D8.part1
    { day: 8, part: 2 } -> run inputFile D8.part2
    { day: 9, part: 1 } -> run inputFile D9.part1
    { day: 9, part: 2 } -> run inputFile D9.part2
    { day: 10, part: 1 } -> run inputFile D10.part1
    { day: 10, part: 2 } -> run inputFile D10.part2
    { day: 11, part: 1 } -> run inputFile D11.part1
    { day: 11, part: 2 } -> run inputFile D11.part2
    { day: 12, part: 1 } -> run inputFile D12.part1
    { day: 12, part: 2 } -> run inputFile D12.part2
    { day: 13, part: 1 } -> run inputFile D13.part1
    { day: 13, part: 2 } -> run' identity inputFile D13.part2
    { day: 14, part: 1 } -> run inputFile D14.part1
    { day: 14, part: 2 } -> run inputFile D14.part2
    { day: 15, part: 1 } -> run inputFile D15.part1
    { day: 15, part: 2 } -> run inputFile D15.part2
    { day: 16, part: 1 } -> run inputFile D16.part1
    { day: 16, part: 2 } -> run inputFile D16.part2
    { day: 17, part: 1 } -> run inputFile D17.part1
    { day: 17, part: 2 } -> run inputFile D17.part2
    { day: 18, part: 1 } -> run inputFile D18.part1
    { day: 18, part: 2 } -> run inputFile D18.part2
    { day: 19, part: 1 } -> run inputFile D19.part1
    { day: 19, part: 2 } -> run inputFile D19.part2
    { day: 20, part: 1 } -> run inputFile D20.part1
    { day: 20, part: 2 } -> run inputFile D20.part2
    { day: 21, part: 1 } -> run inputFile D21.part1
    { day: 21, part: 2 } -> run inputFile D21.part2
    { day: 22, part: 1 } -> run inputFile D22.part1
    { day: 22, part: 2 } -> run inputFile D22.part2
    { day: 23, part: 1 } -> run inputFile D23.part1
    { day: 23, part: 2 } -> run inputFile D23.part2
    { day: 24, part: 1 } -> run inputFile D24.part1
    { day: 24, part: 2 } -> run inputFile D24.part2
    { day: 25, part: 1 } -> run inputFile D25.part1
    { day: 25, part: 2 } -> run inputFile D25.part2
    _ -> throw "Invalid puzzle day or part specified"

run :: forall a. Show a => FilePath -> (String -> Either String a) -> Effect Unit
run = run' show

run' :: forall a. (a -> String) -> FilePath -> (String -> Either String a) -> Effect Unit
run' show' inputFilePath solver =
  catchException handler do
    input <- readTextFile UTF8 inputFilePath
    case solver input of
      Right a -> log $ show' a
      Left err -> warn ("Could not find solution: " <> err) *> exit 1
  where
  handler err = warn ("An exception occurred: " <> fromMaybe (name err <> ": " <> message err) (stack err)) *> exit 1
