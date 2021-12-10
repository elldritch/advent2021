module Advent2021.Puzzles.D3
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser)
import Control.Alternative ((<|>))
import Data.Either (Either, note)
import Data.Foldable (foldl, foldr, product)
import Data.Int (pow)
import Data.List.NonEmpty (NonEmptyList, filterM, head, index, length, zip)
import Data.List.NonEmpty as NEList
import Data.Tuple (Tuple(..), fst, uncurry)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (many1)

newtype Bit
  = Bit Boolean

derive newtype instance eqBit :: Eq Bit

_1 :: Bit
_1 = Bit true

_0 :: Bit
_0 = Bit false

bitP :: Parser Bit
bitP = (char '1' *> pure (Bit true)) <|> (char '0' *> pure (Bit false))

type BitString
  = NonEmptyList Bit

bitstringP :: Parser BitString
bitstringP = many1 bitP

toInt :: BitString -> Int
toInt bits =
  fst
    $ foldr
        (\(Bit b) (Tuple acc i) -> (Tuple (acc + if b then 2 `pow` i else 0) (i + 1)))
        (Tuple 0 0)
        bits

type BitCounts
  = { ones :: Int, zeros :: Int }

type BitStringCounts
  = NonEmptyList BitCounts

countBitOccurrences :: NonEmptyList BitString -> BitStringCounts
countBitOccurrences bitstrings = foldl countBitString zeroCounts bitstrings
  where
  zeroCounts :: BitStringCounts
  zeroCounts = map (const { ones: 0, zeros: 0 }) $ head bitstrings

  addCounts :: BitCounts -> BitCounts -> BitCounts
  addCounts a b = { ones: a.ones + b.ones, zeros: a.zeros + b.zeros }

  countBitString :: BitStringCounts -> BitString -> BitStringCounts
  countBitString counts bitstring = map (uncurry addCounts) $ zip counts $ bitStringToCounts bitstring

  bitStringToCounts :: BitString -> BitStringCounts
  bitStringToCounts bitstring = map bitToCounts bitstring

  bitToCounts :: Bit -> BitCounts
  bitToCounts (Bit true) = { ones: 1, zeros: 0 }

  bitToCounts (Bit false) = { ones: 0, zeros: 1 }

run :: (NonEmptyList BitString -> Either String Int) -> String -> Either String Int
run solver input = runParser inputP input >>= solver
  where
  inputP :: Parser (NonEmptyList BitString)
  inputP = many1 (bitstringP <* newline) <* eof

part1 :: String -> Either String Int
part1 =
  run \bitstrings ->
    pure $ product
      $ map (\f -> toInt $ f $ countBitOccurrences bitstrings) [ epsilonBitString, gammaBitString ]
  where
  gammaBitString :: BitStringCounts -> BitString
  gammaBitString = map (\{ ones, zeros } -> if ones > zeros then (Bit true) else (Bit false))

  epsilonBitString :: BitStringCounts -> BitString
  epsilonBitString = map (\{ ones, zeros } -> if ones < zeros then (Bit true) else (Bit false))

part2 :: String -> Either String Int
part2 =
  run \bitstrings -> do
    o <- oxygenGeneratorRating bitstrings
    c <- co2ScrubberRating bitstrings
    pure $ toInt o * toInt c
  where
  findRating ::
    (NonEmptyList BitString -> Int -> Either String (NonEmptyList BitString)) ->
    NonEmptyList BitString ->
    Either String BitString
  findRating pick bitstrings = head <$> findRatingRecurse pick 0 bitstrings

  -- Is there a combinator for this?
  findRatingRecurse ::
    (NonEmptyList BitString -> Int -> Either String (NonEmptyList BitString)) ->
    Int ->
    NonEmptyList BitString ->
    Either String (NonEmptyList BitString)
  findRatingRecurse pick i bitstrings = do
    next <- pick bitstrings i
    if length next == 1 then pure next else findRatingRecurse pick (i + 1) next

  pickByBit :: (BitCounts -> Bit) -> NonEmptyList BitString -> Either String BitString
  pickByBit pickBit =
    findRating \bitstrings i -> do
      counts <- note "Impossible: bitstring index has no corresponding count" $ index (countBitOccurrences bitstrings) i
      passed <-
        filterM
          ( \bitstring -> do
              bit <- note "Impossible: bitstring index out of bounds" $ index bitstring i
              pure $ bit == pickBit counts
          )
          bitstrings
      note "Impossible: no bitstring selected" $ NEList.fromList passed

  oxygenGeneratorRating :: NonEmptyList BitString -> Either String BitString
  oxygenGeneratorRating =
    pickByBit \{ zeros, ones } -> case compare zeros ones of
      EQ -> _1
      LT -> _1
      GT -> _0

  co2ScrubberRating :: NonEmptyList BitString -> Either String BitString
  co2ScrubberRating =
    pickByBit \{ zeros, ones } -> case compare zeros ones of
      EQ -> _0
      LT -> _0
      GT -> _1
