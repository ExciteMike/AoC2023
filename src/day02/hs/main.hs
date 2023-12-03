-- ghc .\src\day02\hs\main.hs ; .\src\day02\hs\main
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Maybes (mapMaybe)

neededCubes s = neededCubes' (0, 0, 0) s
  where
    neededCubes' cubes "" = cubes
    neededCubes' (r, g, b) s = neededCubes' (r', g', b') s''
      where
        (numString, s') = break (== ' ') $ dropWhile (not . isDigit) s
        num = read numString
        update (r, g, b) s
          | "red" `isPrefixOf` s = ((max r num, g, b), drop 3 s)
          | "green" `isPrefixOf` s = ((r, max g num, b), drop 5 s)
          | "blue" `isPrefixOf` s = ((r, g, max b num), drop 4 s)
          | otherwise = error $ "parse error: \"" ++ s ++ "\""
        ((r', g', b'), s'') = update (r, g, b) $ drop 1 s'

p1 lines = sum $ mapMaybe score_line lines
  where
    score_line line
      | "Game " `isPrefixOf` line = score_line $ drop 5 line
      | otherwise = do
        let (id, rest) = break (== ':') line
        let (r, g, b) = neededCubes $ drop 1 rest
        if (r > 12) || (g > 13) || (b > 14)
          then Nothing
          else Just $ read id

p2 lines = sum $ map scoreLine lines
  where
    scoreLine line = r * g * b
      where
        (r, g, b) = neededCubes $ drop 2 $ dropWhile (/= ':') line

main :: IO ()
main = do
  input <- readFile "puzzle_input/day02"
  print (p1 $ lines input) -- 2156
  print (p2 $ lines input) -- 66909