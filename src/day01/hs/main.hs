-- ghc .\src\day01\hs\main.hs ; .\src\day01\hs\main
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

p1Line :: String -> Int
p1Line line = a * 10 + b
  where
    digits = [ digitToInt c | c <- line, isDigit c ]
    a = head digits
    b = last digits

p2Digits:: String -> [Int]
p2Digits [] = []
p2Digits ('0':s) = 0 : p2Digits s
p2Digits ('1':s) = 1 : p2Digits s
p2Digits ('2':s) = 2 : p2Digits s
p2Digits ('3':s) = 3 : p2Digits s
p2Digits ('4':s) = 4 : p2Digits s
p2Digits ('5':s) = 5 : p2Digits s
p2Digits ('6':s) = 6 : p2Digits s
p2Digits ('7':s) = 7 : p2Digits s
p2Digits ('8':s) = 8 : p2Digits s
p2Digits ('9':s) = 9 : p2Digits s
p2Digits s
  | "one" `isPrefixOf` s = 1 : p2Digits (drop 3 s)
  | "two" `isPrefixOf` s = 2 : p2Digits (drop 3 s)
  | "three" `isPrefixOf` s = 3 : p2Digits (drop 5 s)
  | "four" `isPrefixOf` s = 4 : p2Digits (drop 4 s)
  | "five" `isPrefixOf` s = 5 : p2Digits (drop 4 s)
  | "six" `isPrefixOf` s = 6 : p2Digits (drop 3 s)
  | "seven" `isPrefixOf` s = 7 : p2Digits (drop 5 s)
  | "eight" `isPrefixOf` s = 8 : p2Digits (drop 5 s)
  | "nine" `isPrefixOf` s = 9 : p2Digits (drop 4 s)
  | otherwise = p2Digits (drop 1 s)

p2Line :: String -> Int
p2Line line = a * 10 + b
  where
    digits = p2Digits line
    a = head digits
    b = last digits

p1 :: [String] -> Int
p1 lines = sum $ map p1Line lines

p2 :: [String] -> Int
p2 lines = sum $ map p2Line lines

main :: IO ()
main = do
  input <- readFile "puzzle_input/day01"
  putStrLn $ ("part 1: " ++) $ show $ p1 $ lines input
  putStrLn $ ("part 2: " ++) $ show $ p2 $ lines input