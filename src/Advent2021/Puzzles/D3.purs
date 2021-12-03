module Advent2021.Puzzles.D3
  ( part1
  ) where

import Prelude
import Advent2021.Parsers (newline)
import Control.Alternative ((<|>))
import Data.Array (fromFoldable, zip)
import Data.Array.NonEmpty (fromFoldable1, head)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either)
import Data.Foldable (foldl, foldr)
import Data.Int (pow)
import Data.Tuple (Tuple(..), fst, uncurry)
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodePoints (char, eof)
import Text.Parsing.StringParser.Combinators (many, many1)

newtype Bit
  = Bit Boolean

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

part1 :: String -> Either ParseError Int
part1 input = do
  bitstrings <- runParser inputP input
  let
    counts = countBitOccurrences bitstrings

    gamma = toInt $ gammaBitString counts

    epsilon = toInt $ epsilonBitString counts
  pure $ gamma * epsilon
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

  gammaBitString :: BitStringCounts -> BitString
  gammaBitString = map (\{ ones, zeros } -> if ones > zeros then (Bit true) else (Bit false))

  epsilonBitString :: BitStringCounts -> BitString
  epsilonBitString = map (\{ ones, zeros } -> if ones < zeros then (Bit true) else (Bit false))
