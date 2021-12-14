module Advent2021.Puzzles.D8
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser, space, word)
import Data.Either (Either, note)
import Data.Foldable (class Foldable, foldr, sum, length)
import Data.Int as Int
import Data.List (List, any, concat)
import Data.List as List
import Data.Map as Map
import Data.Set (Set, difference, intersection, subset)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (eof, satisfy)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy)

type Signal
  = Set Char

signalP :: Parser Signal
signalP = Set.fromFoldable <$> many1 segmentP
  where
  segmentP :: Parser Char
  segmentP = satisfy \c -> c >= 'a' && c <= 'g'

type Display
  = { uniques :: List Signal, outputs :: List Signal }

displayP :: Parser Display
displayP = do
  uniques <- replicateA 10 (signalP <* space)
  _ <- word "|"
  outputs <- replicateA 4 (signalP <* space)
  pure { uniques, outputs }

inputP :: Parser (List Display)
inputP = sepEndBy displayP newline <* eof

part1 :: String -> Either String Int
part1 input = do
  displays <- runParser inputP input
  pure
    $ length
    $ concat
    $ map
        (\{ outputs } -> List.filter (\signal -> any (\n -> Set.size signal == n) [ 2, 4, 3, 7 ]) outputs)
        displays

part2 :: String -> Either String Int
part2 input = do
  displays <- runParser inputP input
  outputs <- sequence $ decipherDisplay <$> displays
  pure $ sum outputs
  where
  decipherDisplay :: Display -> Either String Int
  decipherDisplay { uniques, outputs } = do
    one <- find' "1" ((_ == 2) <<< Set.size) uniques
    four <- find' "4" ((_ == 4) <<< Set.size) uniques
    seven <- find' "7" ((_ == 3) <<< Set.size) uniques
    eight <- find' "8" ((_ == 7) <<< Set.size) uniques
    let
      fiveSegmentSignals = List.filter ((_ == 5) <<< Set.size) uniques
    three <- find' "3" (one `subset` _) fiveSegmentSignals
    let
      d = three `difference` one `intersection` four

      b = four `difference` one `difference` d
    five <- find' "5" (b `subset` _) fiveSegmentSignals
    two <- find' "2" (\signal -> signal /= five && signal /= three) fiveSegmentSignals
    let
      sixSegmentSignals = List.filter ((_ == 6) <<< Set.size) uniques
    six <- find' "6" (\signal -> not $ one `subset` signal) sixSegmentSignals
    zero <- find' "0" (\signal -> not $ d `subset` signal) sixSegmentSignals
    nine <- find' "9" (\signal -> signal /= six && signal /= zero) sixSegmentSignals
    let
      signalToDigit =
        Map.fromFoldable
          [ Tuple one 1
          , Tuple two 2
          , Tuple three 3
          , Tuple four 4
          , Tuple five 5
          , Tuple six 6
          , Tuple seven 7
          , Tuple eight 8
          , Tuple nine 9
          , Tuple zero 0
          ]
    digits <-
      note "Impossible: output signal not seen before"
        $ sequence
        $ map (_ `Map.lookup` signalToDigit) outputs
    pure $ digitsToInt digits

  find' :: String -> (Signal -> Boolean) -> List Signal -> Either String Signal
  find' msg pick xs = note ("Invalid input: display does not contain " <> msg) $ List.find pick xs

  digitsToInt :: forall f. Foldable f => f Int -> Int
  digitsToInt digits = _.accumulated $ foldr f z digits
    where
    z = { accumulated: 0, place: 0 }

    f digit { accumulated, place } = { accumulated: accumulated + digit * Int.pow 10 place, place: place + 1 }
