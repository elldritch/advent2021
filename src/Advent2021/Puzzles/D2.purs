module Advent2021.Puzzles.D2
  ( part1
  , part2
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

run :: forall a b. (a -> SubmarineCommand -> a) -> a -> (a -> b) -> String -> Either ParseError b
run move zero finalize input = do
  commands <- runParser courseP input
  pure $ finalize $ foldl move zero commands

product :: forall r. { horizontal :: Int, depth :: Int | r } -> Int
product { horizontal, depth } = horizontal * depth

part1 :: String -> Either ParseError Int
part1 = run move { depth: 0, horizontal: 0 } product
  where
  move pos@{ horizontal } (Forward x) = pos { horizontal = horizontal + x }

  move pos@{ depth } (Down x) = pos { depth = depth + x }

  move pos@{ depth } (Up x) = pos { depth = depth - x }

part2 :: String -> Either ParseError Int
part2 = run move { depth: 0, horizontal: 0, aim: 0 } product
  where
  move { horizontal, depth, aim } (Forward x) =
    { horizontal: horizontal + x
    , depth: depth + aim * x
    , aim: aim
    }

  move pos@{ aim } (Down x) = pos { aim = aim + x }

  move pos@{ aim } (Up x) = pos { aim = aim - x }
