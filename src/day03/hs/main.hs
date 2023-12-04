-- ghc .\src\day03\hs\main.hs ; .\src\day03\hs\main

import Data.Char (digitToInt, isDigit)
import Data.Int (Int64)
import Data.Set (Set, insert, member, toList)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)

data PartNumber = PartNumber
  { row :: Int64,
    start :: Int64,
    end :: Int64,
    value :: Int64
  }
  deriving (Eq, Ord, Show)

data Parts = Parts
  { numbers :: [PartNumber],
    symbols :: Set (Int64, Int64),
    gears :: Set (Int64, Int64)
  }

defaultParts = Parts {numbers = [], symbols = Set.empty, gears = Set.empty}

parse1 :: Parts -> Int64 -> Int64 -> String -> Parts
parse1 parts _ _ "" = parts
parse1 parts row col ('.' : rest) = parse1 parts row (col + 1) rest
parse1 parts row col ('\n' : rest) = parse1 parts (row + 1) 0 rest
parse1 parts row col (c : rest)
  | isDigit c =
    let v = (fromIntegral $ digitToInt c)
        n = PartNumber {row = row, start = col, end = col, value = v}
     in parseInNumber parts row (col + 1) n rest
  | c == '*' =
    let symbols' = insert (row, col) (symbols parts)
        gears' = insert (row, col) (gears parts)
        parts' = (parts {symbols = symbols', gears = gears'})
     in parse1 parts' row (col + 1) rest
  | otherwise =
    let symbols' = insert (row, col) (symbols parts)
        parts' = parts {symbols = symbols'}
     in parse1 parts' row (col + 1) rest

parseInNumber :: Parts -> Int64 -> Int64 -> PartNumber -> String -> Parts
parseInNumber parts row col n "" =
  let numbers' = n : numbers parts
   in parts {numbers = numbers'}
parseInNumber parts row col n ('.' : rest) =
  let numbers' = n : numbers parts
      parts' = parts {numbers = numbers'}
   in parse1 parts' row (col + 1) rest
parseInNumber parts row col n (c : rest)
  | isDigit c =
    let value' = value n * 10 + fromIntegral (digitToInt c)
        n' = n {value = value', end = col}
     in parseInNumber parts row (col + 1) n' rest
  | otherwise =
    let numbers' = n : numbers parts
        parts' = parts {numbers = numbers'}
     in parse1 parts' row col (c : rest)

parse :: String -> Parts
parse = parse1 defaultParts 0 0

p1Filter :: Set (Int64, Int64) -> PartNumber -> Bool
p1Filter symbols PartNumber {row = row, start = start, end = end, value = _} =
  let locations = [(x, y) | x <- [start -1 .. end + 1], y <- [row -1 .. row + 1]]
   in any (`member` symbols) locations

p1Value :: Set (Int64, Int64) -> PartNumber -> Int64
p1Value symbols number =
  if any (`member` symbols) locations then value else 0
  where
    PartNumber {row = row, start = start, end = end, value = value} = number
    locations = [(y, x) | x <- [start -1 .. end + 1], y <- [row -1 .. row + 1]]

p2Value :: [PartNumber] -> (Int64, Int64) -> Int64
p2Value numbers (gearRow, gearCol) = v numbers'
  where
    numbers' = filter filterFunc numbers
    filterFunc PartNumber {row = row, start = start, end = end, value = _} =
      (row-1 <= gearRow) && (gearRow <= row+1) && (start-1 <= gearCol) && (gearCol <= end+1)
    v [a, b] = value a * value b
    v _ = 0

main :: IO ()
main = do
  input <- readFile "puzzle_input/day03"
  let parts = parse input
  let Parts {numbers = numbers, symbols = symbols, gears = gears} = parts
  print $ sum $ map (p1Value symbols) numbers -- 527369
  print $ sum $ map (p2Value numbers) $ toList gears -- 73074886