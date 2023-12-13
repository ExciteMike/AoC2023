import Data.Int (Int64)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Maybes (fromJust, mapMaybe)
import Data.List(dropWhileEnd)

-- ghc -O0 .\src\day13\hs\main.hs
-- .\src\day13\hs\main

data Pattern = Pattern
  { mirrors :: Set (Int64, Int64),
    width :: Int64,
    height :: Int64
  }
  deriving (Eq, Ord, Show)

checkHReflect :: Pattern -> Int64 -> (Int64, Int64) -> Bool
checkHReflect pat i (x, y) = outOfBounds || reflectionPresent
  where
    y' = 2 * i - y - 1
    outOfBounds = (y' < 0) || (y' >= height pat)
    reflectionPresent = Set.member (x, y') (mirrors pat)

checkVReflect :: Pattern -> Int64 -> (Int64, Int64) -> Bool
checkVReflect pat i (x, y) = outOfBounds || reflectionPresent
  where
    x' = 2 * i - x - 1
    outOfBounds = (x' < 0) || (x' >= width pat)
    reflectionPresent = Set.member (x', y) (mirrors pat)

findHReflectionLine :: Int64 -> Pattern -> Maybe Int64
findHReflectionLine missing p = search 1
  where
    target = Set.size $ mirrors p
    check i = fromIntegral $
      length $
        [ () | p1 <- Set.toList $ mirrors p, checkHReflect p i p1
        ]
    search i
      | (i < height p) && (fromIntegral target == (missing + check i)) = Just i
      | i < height p = search $ i + 1
      | otherwise = Nothing

findVReflectionLine :: Int64 -> Pattern -> Maybe Int64
findVReflectionLine missing p = search 1
  where
    target = Set.size $ mirrors p
    check i = fromIntegral $
      length $
        [ () | p1 <- Set.toList $ mirrors p, checkVReflect p i p1
        ]
    search i
      | (i < width p) && (fromIntegral target == (missing + check i)) = Just i
      | i < width p = search $ i + 1
      | otherwise = Nothing

fromString :: String -> Pattern
fromString s = parse' [] 0 0 0 s'
  where
    s' = dropWhileEnd (=='\n') s
    parse' coords w _ h "" = Pattern {mirrors = Set.fromList coords, width = w + 1, height = h + 1}
    parse' coords w _ y ('\n' : cs) = parse' coords w 0 (y + 1) cs
    parse' coords w x y ('.' : cs) = parse' coords (max x w) (x + 1) y cs
    parse' coords w x y ('#' : cs) = parse' ((x, y) : coords) (max x w) (x + 1) y cs
    parse' _ _ _ _ _ = error "unhandled parse' case"

summarize :: Pattern -> Int64
summarize p =
  case findVReflectionLine 0 p of
    Just x -> x
    Nothing -> 100 * fromJust (findHReflectionLine 0 p)

summarize2 :: Pattern -> Int64
summarize2 p =
  case findVReflectionLine 1 p of
    Just x -> x
    Nothing -> 100 * fromJust (findHReflectionLine 1 p)

main = do
  input <- readFile "puzzle_input/day13"
  let blocks = map fromString $ splitOn "\n\n" input
  -- 33122
  print $ sum $ map summarize blocks
  -- 32312
  print $ sum $ map summarize2  blocks