import Data.List (dropWhileEnd, isPrefixOf)
import Data.Set (Set)
import Data.Set qualified as Set

-- ghc .\src\day13\hs\main.hs
-- .\src\day13\hs\main

blocks :: String -> [String]
blocks = block' ""
  where
    block' "" "" = []
    block' curBlock "" = [curBlock]
    block' "" ('\n' : '\n' : rest) = block' "" rest
    block' curBlock ('\n' : '\n' : rest) = curBlock : block' "" rest
    block' curBlock (c : rest) = block' (curBlock ++ [c]) rest

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error ":("

data Pattern = Pattern
  { mirrors :: Set (Int, Int),
    width :: Int,
    height :: Int
  }
  deriving (Eq, Ord, Show)

checkHReflect :: Pattern -> Int -> (Int, Int) -> Bool
checkHReflect pat i (x, y) = outOfBounds || reflectionPresent
  where
    y' = 2 * i - y - 1
    outOfBounds = (y' < 0) || (y' >= height pat)
    reflectionPresent = Set.member (x, y') (mirrors pat)

checkVReflect :: Pattern -> Int -> (Int, Int) -> Bool
checkVReflect pat i (x, y) = outOfBounds || reflectionPresent
  where
    x' = 2 * i - x - 1
    outOfBounds = (x' < 0) || (x' >= width pat)
    reflectionPresent = Set.member (x', y) (mirrors pat)

findHReflectionLine :: Int -> Pattern -> Maybe Int
findHReflectionLine missing p = search 1
  where
    target = Set.size $ mirrors p
    check i =
      fromIntegral $
        length $
          [ () | p1 <- Set.toList $ mirrors p, checkHReflect p i p1
          ]
    search i
      | (i < height p) && (fromIntegral target == (missing + check i)) = Just i
      | i < height p = search $ i + 1
      | otherwise = Nothing

findVReflectionLine :: Int -> Pattern -> Maybe Int
findVReflectionLine missing p = search 1
  where
    target = Set.size $ mirrors p
    check i =
      fromIntegral $
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
    s' = dropWhileEnd (== '\n') s
    parse' coords w _ h "" = Pattern {mirrors = Set.fromList coords, width = w + 1, height = h + 1}
    parse' coords w _ y ('\n' : cs) = parse' coords w 0 (y + 1) cs
    parse' coords w x y ('.' : cs) = parse' coords (max x w) (x + 1) y cs
    parse' coords w x y ('#' : cs) = parse' ((x, y) : coords) (max x w) (x + 1) y cs
    parse' _ _ _ _ _ = error "unhandled parse' case"

summarize :: Pattern -> Int
summarize p =
  case findVReflectionLine 0 p of
    Just x -> x
    Nothing -> 100 * fromJust (findHReflectionLine 0 p)

summarize2 :: Pattern -> Int
summarize2 p =
  case findVReflectionLine 1 p of
    Just x -> x
    Nothing -> 100 * fromJust (findHReflectionLine 1 p)

main = do
  input <- readFile "puzzle_input/day13"
  let patterns = map fromString $ blocks input
  print $ sum $ map summarize patterns -- 33122
  print $ sum $ map summarize2 patterns -- 32312