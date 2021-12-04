module Advent2021.Puzzles.D3
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Debug (undefined)
import Advent2021.Parsers (newline, runParser)
import Control.Alternative ((<|>))
import Data.Array (fromFoldable, length, unsafeIndex, zip)
import Data.Array.NonEmpty (filter, fromArray, fromFoldable1, head, range)
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either)
import Data.Foldable (foldl, foldr, product)
import Data.Int (pow)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, uncurry)
import Debug (spy)
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

-- TODO: Remove unsafe usage
-- - Runner should generally be returning Either String Int, for other kinds of errors
-- - Solver should emit Either in case of invalid input
-- - Spec handler needs to be fixed
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
    $ \bitstrings ->
        pure $ product $ map (\f -> toInt $ f bitstrings) [ oxygenGeneratorRating, co2ScrubberRating ]
  where
  findRating ::
    (NonEmptyArray BitString -> Int -> NonEmptyArray BitString) ->
    NonEmptyArray BitString ->
    BitString
  findRating pick bitstrings = head $ findRatingRecurse pick 0 bitstrings

  -- Is there a combinator for this?
  findRatingRecurse ::
    (NonEmptyArray BitString -> Int -> NonEmptyArray BitString) ->
    Int ->
    NonEmptyArray BitString ->
    NonEmptyArray BitString
  findRatingRecurse pick i bitstrings = let next = pick bitstrings i in if NE.length next == 1 then next else findRatingRecurse pick (i + 1) next

  -- TODO: the problem is that I need to recompute counts on every iteration
  oxygenGeneratorRating :: NonEmptyArray BitString -> BitString
  oxygenGeneratorRating =
    findRating
      $ \bitstrings i ->
          let
            { zeros, ones } = unsafePartial $ unsafeIndex (countBitOccurrences bitstrings) i
          in
            unsafePartial $ fromJust $ fromArray
              $ filter
                  ( \bitstring ->
                      unsafeIndex bitstring i
                        == case compare zeros ones of
                            EQ -> _1
                            LT -> _1
                            GT -> _0
                  )
                  bitstrings

  co2ScrubberRating :: NonEmptyArray BitString -> BitString
  co2ScrubberRating =
    findRating
      $ \bitstrings i ->
          let
            { zeros, ones } = unsafePartial $ unsafeIndex (countBitOccurrences bitstrings) i
          in
            unsafePartial $ fromJust $ fromArray
              $ filter
                  ( \bitstring ->
                      unsafeIndex bitstring i
                        == case compare zeros ones of
                            EQ -> _0
                            LT -> _0
                            GT -> _1
                  )
                  bitstrings
