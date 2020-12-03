import Data.List (foldl, break, (!!))
import Debug.Trace (trace)

countTrees :: Int -> Int -> Int -> [String] -> Int
countTrees right down step m | row >= length m = 0
                             | (m!!row)!!col == '#' = 1 + countTrees right down (step + 1) m
                             | otherwise = countTrees right down (step + 1) m
  where row = step * down
        col = (step * right) `mod` (length (m!!row))

-- main = interact $ (++ "\n") . show . countTrees 3 1 0 . lines

ans2 m = countTrees 1 1 0 m * countTrees 3 1 0 m * countTrees 5 1 0 m * countTrees 7 1 0 m * countTrees 1 2 0 m
main = interact $ (++ "\n") . show . ans2 . lines
  
