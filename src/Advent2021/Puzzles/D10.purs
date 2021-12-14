module Advent2021.Puzzles.D10
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser)
import Control.Alternative ((<|>))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), hush, note)
import Data.Foldable (foldl, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), index, mapMaybe, sort, uncons, (:))
import Data.List.NonEmpty as NEList
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, sum)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

data Style
  = Paren
  | Square
  | Curly
  | Angle

derive instance eqStyle :: Eq Style

derive instance genericStyle :: Generic Style _

instance showStyle :: Show Style where
  show = genericShow

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
lineP = NEList.toList <$> many1 tokenP

type Program
  = List Line

programP :: Parser Program
programP = sepEndBy lineP newline

type ChunkStack
  = List Style

type SyntaxError
  = { expected :: Maybe Token, actual :: Token }

check :: Line -> Either SyntaxError ChunkStack
check line = checkR Nil line
  where
  checkR :: ChunkStack -> Line -> Either SyntaxError ChunkStack
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
    Nothing -> pure stack

run :: forall a. (List (Either SyntaxError ChunkStack) -> Either String a) -> String -> Either String a
run pointsOfProgram input = do
  program <- runParser (programP <* eof) input
  score <- pointsOfProgram $ check <$> program
  pure $ score

part1 :: String -> Either String Int
part1 =
  run \results -> do
    let
      corrupted =
        mapMaybe
          ( case _ of
              Right _ -> Nothing
              Left err -> Just err
          )
          results
    scores <- sequence $ syntaxErrorScore <$> corrupted
    pure $ sum scores
  where
  syntaxErrorScore :: SyntaxError -> Either String Int
  syntaxErrorScore { actual } = case actual of
    Open _ -> Left "Impossible: opening tokens are never a syntax error"
    Close Paren -> Right 3
    Close Square -> Right 57
    Close Curly -> Right 1197
    Close Angle -> Right 25137

part2 :: String -> Either String BigInt
part2 =
  run \results ->
    let
      incomplete = mapMaybe hush $ results

      scores = autoCompleteScore <$> incomplete
    in
      note "Impossible: index out-of-bounds when retrieving middle score"
        $ index (sort scores) (length scores / 2)
  where
  autoCompleteScore :: ChunkStack -> BigInt
  autoCompleteScore =
    foldl
      ( \score style ->
          score * (BigInt.fromInt 5)
            + case style of
                Paren -> BigInt.fromInt 1
                Square -> BigInt.fromInt 2
                Curly -> BigInt.fromInt 3
                Angle -> BigInt.fromInt 4
      )
      $ BigInt.fromInt 0
