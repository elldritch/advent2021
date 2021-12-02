module Advent2021.Puzzles.D2
  ( part1
  ) where

import Prelude

import Advent2021.Parsers (integer, newline, word)
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.List (List, foldl)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (many)

data SubmarineCommand
  = Forward Int
  | Down Int
  | Up Int

commandP :: Parser SubmarineCommand
commandP = forwardP <|> downP <|> upP
  where
  forwardP = Forward <$> (word "forward" *> integer)

  downP = Down <$> (word "down" *> integer)

  upP = Up <$> (word "up" *> integer)

courseP :: Parser (List SubmarineCommand)
courseP = many (commandP <* newline) <* eof

part1 :: String -> Either ParseError { depth :: Int, horizontal :: Int }
part1 s = do
  commands <- runParser courseP s
  pure $ foldl move { depth: 0, horizontal: 0 } commands
  where
  move pos@{ horizontal } (Forward x) = pos { horizontal = horizontal + x }

  move pos@{ depth } (Down x) = pos { depth = depth + x }

  move pos@{ depth } (Up x) = pos { depth = depth - x }
