module Advent2021.Puzzles.D3
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline, runParser)
import Control.Alternative ((<|>))
import Data.Array (fromFoldable, unsafeIndex, zip)
import Data.Array.NonEmpty (filter, fromArray, fromFoldable1, head)
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either)
import Data.Foldable (foldl, foldr, product)
import Data.Int (pow)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, uncurry)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (many, many1)

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
  = Array Bit

bitstringP :: Parser BitString
bitstringP = fromFoldable <$> many bitP

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
  = Array BitCounts

countBitOccurrences :: NonEmptyArray BitString -> BitStringCounts
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

run :: (NonEmptyArray BitString -> Either String Int) -> String -> Either String Int
run solver input = runParser inputP input >>= solver
  where
  inputP :: Parser (NonEmptyArray BitString)
  inputP = fromFoldable1 <$> many1 (bitstringP <* newline) <* eof

part1 :: String -> Either String Int
part1 =
  run
    $ \bitstrings ->
        pure $ product
          $ map (\f -> toInt $ f $ countBitOccurrences bitstrings) [ epsilonBitString, gammaBitString ]
  where
  gammaBitString :: BitStringCounts -> BitString
  gammaBitString = map (\{ ones, zeros } -> if ones > zeros then (Bit true) else (Bit false))

  epsilonBitString :: BitStringCounts -> BitString
  epsilonBitString = map (\{ ones, zeros } -> if ones < zeros then (Bit true) else (Bit false))

part2 :: String -> Either String Int
part2 =
  run
    $ \bitstrings -> do
        o <- oxygenGeneratorRating bitstrings
        c <- co2ScrubberRating bitstrings
        pure $ toInt o * toInt c
  where
  findRating ::
    (NonEmptyArray BitString -> Int -> Either String (NonEmptyArray BitString)) ->
    NonEmptyArray BitString ->
    Either String BitString
  findRating pick bitstrings = head <$> findRatingRecurse pick 0 bitstrings

  -- Is there a combinator for this?
  findRatingRecurse ::
    (NonEmptyArray BitString -> Int -> Either String (NonEmptyArray BitString)) ->
    Int ->
    NonEmptyArray BitString ->
    Either String (NonEmptyArray BitString)
  findRatingRecurse pick i bitstrings = do
    next <- pick bitstrings i
    if NE.length next == 1 then pure next else findRatingRecurse pick (i + 1) next

  -- TODO: how do I get rid of unsafes here?
  -- Maybe do an unfold and pick the last one to avoid unsafe indexing?
  oxygenGeneratorRating :: NonEmptyArray BitString -> Either String BitString
  oxygenGeneratorRating =
    findRating
      $ \bitstrings i ->
          unsafePartial $ pure
            $ let
                -- Maybe I can avoid this one by just counting the single column of bits?
                { zeros, ones } = unsafeIndex (countBitOccurrences bitstrings) i
              in
                -- How do I avoid this one?
                fromJust $ fromArray
                  $ filter
                      ( \bitstring ->
                          -- How do I avoid this one?
                          unsafeIndex bitstring i
                            == case compare zeros ones of
                                EQ -> _1
                                LT -> _1
                                GT -> _0
                      )
                      bitstrings

  co2ScrubberRating :: NonEmptyArray BitString -> Either String BitString
  co2ScrubberRating =
    findRating
      $ \bitstrings i ->
          unsafePartial $ pure
            $ let
                { zeros, ones } = unsafeIndex (countBitOccurrences bitstrings) i
              in
                fromJust $ fromArray
                  $ filter
                      ( \bitstring ->
                          unsafeIndex bitstring i
                            == case compare zeros ones of
                                EQ -> _0
                                LT -> _0
                                GT -> _1
                      )
                      bitstrings
