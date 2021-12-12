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
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, warn)
import Effect.Exception (catchException, message, throw)
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
    _ -> throw "Invalid puzzle day or part specified"

run :: forall a. Show a => FilePath -> (String -> Either String a) -> Effect Unit
run inputFilePath solver =
  catchException handler do
    input <- readTextFile UTF8 inputFilePath
    case solver input of
      Right a -> log $ show a
      Left err -> warn ("Could not find solution: " <> err) *> exit 1
  where
  handler err = warn ("An exception occurred: " <> message err) *> exit 1
