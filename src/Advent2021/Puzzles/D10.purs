module Advent2021.Puzzles.D10
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), mapMaybe, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence, sum)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (many, sepEndBy)

data Style
  = Paren
  | Square
  | Curly
  | Angle

derive instance eqStyle :: Eq Style

data Token
  = Open Style
  | Close Style

derive instance eqToken :: Eq Token

instance showToken :: Show Token where
  show (Open Paren) = "("
  show (Close Paren) = ")"
  show (Open Square) = "["
  show (Close Square) = "]"
  show (Open Curly) = "{"
  show (Close Curly) = "}"
  show (Open Angle) = "<"
  show (Close Angle) = ">"

tokenP :: Parser Token
tokenP =
  (Open Paren <$ char '(')
    <|> (Close Paren <$ char ')')
    <|> (Open Square <$ char '[')
    <|> (Close Square <$ char ']')
    <|> (Open Curly <$ char '{')
    <|> (Close Curly <$ char '}')
    <|> (Open Angle <$ char '<')
    <|> (Close Angle <$ char '>')

type Line
  = List Token

lineP :: Parser Line
lineP = many tokenP

showLine :: Line -> String
showLine line = String.joinWith "" $ Array.fromFoldable $ map show line

type Program
  = List Line

programP :: Parser Program
programP = sepEndBy lineP newline

showProgram :: Program -> String
showProgram lines = String.joinWith "\n" $ Array.fromFoldable $ map showLine lines

type SyntaxChecker
  = List Style

type SyntaxError
  = { expected :: Maybe Token, actual :: Token }

check :: Line -> Either SyntaxError Unit
check line = checkR Nil line
  where
  checkR :: SyntaxChecker -> Line -> Either SyntaxError Unit
  checkR stack remaining = case uncons remaining of
    Just { head: currentChar, tail: remaining' } -> case currentChar of
      Open style -> checkR (style : stack) remaining'
      Close style -> case uncons stack of
        Just { head: lastOpened, tail: stack' } ->
          if style == lastOpened then
            checkR stack' remaining'
          else
            Left { actual: currentChar, expected: Just (Close lastOpened) }
        Nothing -> Left { actual: currentChar, expected: Nothing }
    Nothing -> pure unit

points :: SyntaxError -> Either String Int
points { actual } = case actual of
  Open _ -> Left "Impossible: opening tokens are never a syntax error"
  Close Paren -> Right 3
  Close Square -> Right 57
  Close Curly -> Right 1197
  Close Angle -> Right 25137

part1 :: String -> Either String Int
part1 input = do
  program <- runParser (programP <* eof) input
  let
    corrupted =
      mapMaybe
        ( \result -> case result of
            Right _ -> Nothing
            Left err -> Just err
        )
        $ map check program
  score <- sequence $ points <$> corrupted
  pure $ sum score

part2 :: String -> Either String Int
part2 input = pure 0
