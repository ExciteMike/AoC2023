-- ghc .\src\day09\hs\main.hs
-- .\src\day09\hs\main

main = do
    input <- readFile "puzzle_input/day09"
    -- 1884768153 1031
    print $ sumPairs $ map (extrapolate . parseLine) (lines input)
  where
    deltas [] = []
    deltas [_] = []
    deltas (x:y:rest) = y - x : deltas (y:rest)
    extrapolate xs
        | all (==0) xs = (0, 0)
        | otherwise = let (fwd, back) = extrapolate $ deltas xs
                       in (last xs + fwd, head xs - back)
    sumPairs = foldr (\(a, b) (c, d) -> (a+c, b+d)) (0, 0)
    parseLine = map read . words