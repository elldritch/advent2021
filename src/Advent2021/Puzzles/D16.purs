module Advent2021.Puzzles.D16
  ( part1
  , part2
  ) where

import Prelude
import Advent2021.Debug (undefined)
import Data.Either (Either)
import Data.List (List)
import Text.Parsing.StringParser (Parser)

data Packet
  = Packet { version :: Int, typeID :: Int, payload :: Payload }

data Payload
  = Literal { value :: Int }
  | Operator { subPackets :: List Packet }

packetP :: Parser Packet
packetP = undefined

part1 :: String -> Either String Int
part1 input = pure 0

part2 :: String -> Either String Int
part2 input = pure 0
