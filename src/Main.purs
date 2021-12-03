module Main
  ( main
  ) where

import Prelude
import Advent2021.Puzzles.D1 as D1
import Advent2021.Puzzles.D2 as D2
import Advent2021.Puzzles.D3 as D3
import Advent2021.Runners (run, runInts)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Path (FilePath)
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
    { day: 1, part: 1 } -> runInts inputFile D1.part1
    { day: 1, part: 2 } -> runInts inputFile D1.part2
    { day: 2, part: 1 } -> run inputFile D2.part1
    { day: 2, part: 2 } -> run inputFile D2.part2
    { day: 3, part: 1 } -> run inputFile D3.part1
    _ -> throw "Invalid puzzle day or part specified"
