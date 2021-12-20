module Advent2021.Bits
  ( Bit(..)
  , BitString
  , _0
  , _1
  , fromBinaryDigit
  , fromBinaryString
  , fromHexDigit
  , fromHexString
  , fromInt
  , padZerosTo
  , toInt
  ) where

import Prelude
import Advent2021.Parsers (digit)
import Control.Alternative ((<|>))
import Data.Foldable (foldr)
import Data.Int (pow)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Unfoldable1 (replicate1)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char)
import Text.Parsing.StringParser.Combinators (many1)

newtype Bit
  = Bit Boolean

derive newtype instance eqBit :: Eq Bit

instance showBit :: Show Bit where
  show (Bit true) = "1"
  show (Bit false) = "0"

_1 :: Bit
_1 = Bit true

_0 :: Bit
_0 = Bit false

fromBinaryDigit :: Parser Bit
fromBinaryDigit = (char '1' $> Bit true) <|> (char '0' $> Bit false)

type BitString
  = NonEmptyList Bit

fromBinaryString :: Parser BitString
fromBinaryString = many1 fromBinaryDigit

fromHexDigit :: Parser BitString
fromHexDigit =
  padZerosTo 4
    <$> fromInt
    <$> ( digit
          <|> (char 'A' $> 10)
          <|> (char 'B' $> 11)
          <|> (char 'C' $> 12)
          <|> (char 'D' $> 13)
          <|> (char 'E' $> 14)
          <|> (char 'F' $> 15)
      )

fromHexString :: Parser BitString
fromHexString = NEList.concat <$> many1 fromHexDigit

toInt :: BitString -> Int
toInt bits =
  _.acc
    $ foldr
        (\(Bit b) { acc, base } -> { acc: acc + if b then 2 `pow` base else 0, base: base + 1 })
        ({ acc: 0, base: 0 })
        bits

fromInt :: Int -> BitString
fromInt n = fromIntR (n / 2) $ NEList.singleton $ if n `mod` 2 == 1 then _1 else _0
  where
  fromIntR :: Int -> BitString -> BitString
  fromIntR 0 bs = bs

  fromIntR x bs = fromIntR (x / 2) $ NEList.cons (if x `mod` 2 == 1 then _1 else _0) bs

padZerosTo :: Int -> BitString -> BitString
padZerosTo width bs =
  if l < width then
    replicate1 (width - l) _0 <> bs
  else
    bs
  where
  l = NEList.length bs
