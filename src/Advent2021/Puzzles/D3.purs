module Advent2021.Puzzles.D3
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Parsers (newline)
import Control.Alternative ((<|>))
import Data.Array (fromFoldable, unsafeIndex, zip)
import Data.Array.NonEmpty (filter, fromArray, fromFoldable1, head)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either)
import Data.Foldable (foldl, foldr, product)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (pow)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..), fst, uncurry)
import Debug (spy)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
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

-- TODO: Remove unsafe usage
-- - Runner should generally be returning Either String Int, for other kinds of errors
-- - Solver should emit Either in case of invalid input
-- - Spec handler needs to be fixed
run :: (NonEmptyArray BitString -> BitStringCounts -> Int) -> String -> Either ParseError Int
run solver input = do
  bitstrings <- runParser inputP input
  pure $ solver bitstrings $ countBitOccurrences bitstrings
  where
  inputP :: Parser (NonEmptyArray BitString)
  inputP = fromFoldable1 <$> many1 (bitstringP <* newline) <* eof

  bitToCounts :: Bit -> BitCounts
  bitToCounts (Bit true) = { ones: 1, zeros: 0 }

  bitToCounts (Bit false) = { ones: 0, zeros: 1 }

  bitStringToCounts :: BitString -> BitStringCounts
  bitStringToCounts bitstring = map bitToCounts bitstring

  countBitOccurrences :: NonEmptyArray BitString -> BitStringCounts
  countBitOccurrences bitstrings = foldl countBitString zeroCounts bitstrings
    where
    zeroCounts :: BitStringCounts
    zeroCounts = map (const { ones: 0, zeros: 0 }) $ head bitstrings

    addCounts :: BitCounts -> BitCounts -> BitCounts
    addCounts a b = { ones: a.ones + b.ones, zeros: a.zeros + b.zeros }

    countBitString :: BitStringCounts -> BitString -> BitStringCounts
    countBitString counts bitstring = map (uncurry addCounts) $ zip counts $ bitStringToCounts bitstring

part1 :: String -> Either ParseError Int
part1 = run $ \_ counts -> product $ map (\f -> toInt $ f counts) [ epsilonBitString, gammaBitString ]
  where
  gammaBitString :: BitStringCounts -> BitString
  gammaBitString = map (\{ ones, zeros } -> if ones > zeros then (Bit true) else (Bit false))

  epsilonBitString :: BitStringCounts -> BitString
  epsilonBitString = map (\{ ones, zeros } -> if ones < zeros then (Bit true) else (Bit false))

part2 :: String -> Either ParseError Int
part2 =
  run
    $ \bitstrings counts ->
        product $ map (\f -> toInt $ f bitstrings counts) [ oxygenGeneratorRating, co2ScrubberRating ]
  where
  -- TODO: should be "get the singleton", not `head`
  findRating :: (Int -> NonEmptyArray BitString -> BitCounts -> NonEmptyArray BitString) -> NonEmptyArray BitString -> BitStringCounts -> BitString
  findRating pick bitstrings counts = head $ foldlWithIndex pick bitstrings $ spy "counts" counts

  -- TODO: the problem is that I need to recompute counts on every iteration
  oxygenGeneratorRating :: NonEmptyArray BitString -> BitStringCounts -> BitString
  oxygenGeneratorRating =
    findRating
      $ \i bitstrings { ones, zeros } ->
          unsafePartial $ fromJust $ spy "oxy" $ fromArray
            $ filter
                ( \bitstring ->
                    unsafeIndex bitstring i
                      == case compare zeros ones of
                          EQ -> _1
                          LT -> _1
                          GT -> _0
                )
                bitstrings

  co2ScrubberRating :: NonEmptyArray BitString -> BitStringCounts -> BitString
  co2ScrubberRating =
    findRating
      $ \i bitstrings { ones, zeros } ->
          unsafePartial $ fromJust $ spy "co2" $ fromArray
            $ filter
                ( \bitstring ->
                    unsafeIndex bitstring i
                      == case compare zeros ones of
                          EQ -> _0
                          LT -> _0
                          GT -> _1
                )
                bitstrings
