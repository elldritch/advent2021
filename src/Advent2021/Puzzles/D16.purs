module Advent2021.Puzzles.D16
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Bits (Bit(..), BitString, bit0P, bit1P, fromBinaryDigit, fromHexString, showBinaryString)
import Advent2021.Bits as Bits
import Advent2021.Debug (spy', spyS')
import Advent2021.Parsers (runParser)
import Data.Either (Either)
import Data.Foldable (foldr, sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NEList
import Data.List.Types (NonEmptyList)
import Data.Unfoldable (replicateA)
import Data.Unfoldable1 (replicate1A)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (eof)
import Text.Parsing.StringParser.Combinators (many)

type TypeID
  = Int

data Packet
  = Packet { version :: Int, typeID :: TypeID, payload :: Payload }

bitsP :: Int -> Parser BitString
bitsP n = replicate1A n fromBinaryDigit

numP :: Int -> Parser Int
numP = map Bits.toInt <<< bitsP

packetP :: Parser { packet :: Packet, length :: Int }
packetP = do
  version <- spy' "version" <$> numP 3
  typeID <- spy' "typeID" <$> numP 3
  { payload, length: payloadLength } <- spy' "payload" <$> payloadP typeID
  pure $ spy' "packet" $ { packet: Packet { version, typeID, payload }, length: payloadLength + 3 + 3 }

data Payload
  = Literal { value :: Int }
  | Operator { subPackets :: List Packet }

payloadP :: TypeID -> Parser { payload :: Payload, length :: Int }
payloadP 4 = do
  groups <- many $ groupP bit1P
  lastGroup <- groupP bit0P
  let
    bits = foldr (<>) lastGroup groups
  pure $ { payload: Literal { value: Bits.toInt bits }, length: (List.length groups + 1) * 5 }
  where
  groupP :: Parser Bit -> Parser BitString
  groupP prefixP = prefixP *> bitsP 4

payloadP _ = do
  lengthTypeID <- spy' "length type ID" <$> fromBinaryDigit
  case lengthTypeID of
    Bit false -> do
      totalSubpacketLength <- spy' "total subpacket length" <$> numP 15
      subPackets <- subPacketsUntilLength totalSubpacketLength
      unless (totalSubpacketLength == sum (_.length <$> subPackets))
        $ fail "Subpacket lengths do not sum to specified total length"
      pure
        $ { payload: Operator { subPackets: _.packet <$> NEList.toList subPackets }
          , length: totalSubpacketLength + 1 + 15
          }
    Bit true -> do
      numPackets <- spy' "subpacket count" <$> numP 11
      subPackets <- replicateA numPackets packetP
      pure
        $ { payload: Operator { subPackets: _.packet <$> subPackets }
          , length: sum (_.length <$> subPackets) + 1 + 11
          }
  where
  subPacketsUntilLength :: Int -> Parser (NonEmptyList { packet :: Packet, length :: Int })
  subPacketsUntilLength l = do
    first <- packetP
    rest <- subPacketsUntilLengthR (l - first.length)
    pure $ NEList.cons' first rest
    where
    subPacketsUntilLengthR :: Int -> Parser (List { packet :: Packet, length :: Int })
    subPacketsUntilLengthR 0 = pure Nil

    subPacketsUntilLengthR l' = do
      packet <- packetP
      rest <- subPacketsUntilLengthR (spy' "remaining" $ l' - packet.length)
      pure $ packet : rest

inputP :: Parser Packet
inputP =
  (spy' "packet" <<< _.packet <$> packetP)
    <* (spyS' "trailing zeros" <$> many bit0P)
    <* (spy' "eof" eof)

part1 :: String -> Either String Int
part1 input = do
  bits <- spyS' "bits" <$> runParser fromHexString $ spy' "hex input" $ input
  message <- spy' "message" <$> runParser inputP $ spy' "binary input" $ showBinaryString bits
  pure $ sumVersions message
  where
  sumVersions :: Packet -> Int
  sumVersions (Packet { version, payload: Literal _ }) = version

  sumVersions (Packet { version, payload: Operator { subPackets } }) = version + sum (sumVersions <$> subPackets)

part2 :: String -> Either String Int
part2 input = pure 0
