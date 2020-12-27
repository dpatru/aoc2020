import Debug.Trace
import Data.Map.Strict as M ((!), insertWith, fromList, Map, empty)
import qualified Data.List as L (sort, map)

traceIdShow :: Show a => a -> a
traceIdShow x = trace (show x) x

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x [] = []
splitOn x (y:xs) | y == x = []: splitOn x xs
                 | otherwise = (y: ys) : yss
  where (ys: yss) = splitOn x xs

diffs :: [Int] -> [Int] -- from sorted list of voltages to differences
diffs [] = []
diffs (x:[]) = [3]
diffs (x:y:xs) = (y-x):(diffs $ y:xs)

countDiffs :: [Int] -> Map Int Int -- from list of differences to Map from difference to count 
countDiffs = foldr (\k m' -> insertWith (+) k 1 m') empty -- $ fromList [(1, 1)]

main = interact $ show
       . part2
       . part1
       -- . traceIdShow
       . diffs
       . ([0] ++)
       . L.sort
       . L.map read
       . lines

part1 :: [Int] -> [Int] -- from diffs to diffs
part1 = tracef
        $ (\m -> "Part 1: " ++ (show (m!1, m!3, m!1 * m!3)))
        . countDiffs

part2 :: [Int] -> Int
part2 ds = let (_,_,_,_,cf) = foldl f (0,0,0,0,1) ds in cf
  -- f updates the (count,jump) history given a new jump.  The
  -- (count,jump) history is a tuple containing a record of the last 3
  -- counts and the intervening jumps. (The notation is c3 for the
  -- count ofter the third jump in the history, j2 for the second jump
  -- in the history, c2 for the count after that, j1 for the
  -- immediately preceding jump, and c1 for the immediately previous
  -- count after the j1 jump.) The count is the number of possible
  -- paths up to that point.  The number of paths after a jump is the
  -- number of paths before the jump, c1, plus possibly the number of
  -- paths before the previous jump and the one before that, if the
  -- jumps were small enough.
  where f (c3,j2,c2,j1,c1) j = traceIdShow (c2,j1,c1,j,c)
          where c | j2+j1+j <= 3 = c3+c2+c1
                  | j1 + j <= 3 = c2+c1
                  | j <= 3 = c1
                  | otherwise
                  = error $ "bad jump values " ++ show (j2,j1,j)


-- diffsToLists :: [Int] -> [[Int]]
-- diffsToLists [] = []
-- diffsToLists x p (d:ds)
--   | d > p
--   = []
--   | otherwise = [x:xs | xs <- diffsToLists 4-
--                    diffsToLists (x+d) (p-d))
-- diffstolists x (p-d) (d:ds) = [(x:xs) | xs <- diffsToLists (x+d) ds]


-- part2 :: [Int] -> Int -- from differences to combinations
-- part2 = tracef (\combinations -> "Part 2: " ++ show combinations) .
--   combs (1,0,0)
--   where combs :: (Int,Int,Int) -> [Int] -> Int
--         -- combs calculates the combinations taking a combination tuple and
--         -- the list of differences
--         -- the combination tupple tracks how many times we reach the next three jumps.
--         combs (c1,c2,c3) [] = 0
--         combs (c1,c2,c3)  [x]
--           = trace ("Just 1 left: " ++ show (c1, x)) $
--           c1
--         combs (c1,c2,c3) [x1,x2]
--           | x1 + x2 < 4
--           = trace ("Two left: Can jump 2: " ++ show (c1, c2, x1,x2)) $
--             combs (c2+c1, c3+c1, 0) [x2]
--           | otherwise
--           = trace ("Two left: Can jump 1: " ++ show (c1, c2, x1,x2)) $
--             combs (c2+c1, c3, 0) [x2]
--         combs (c1,c2,c3) (x1:x2:x3:xs)
--           | x1 + x2 + x3 < 4
--           = trace ("Can jump 3: " ++ show (c1, c2, c3, x1,x2,x3)) $
--             combs (c2+c1, c3+c1, c1) (x2:x3:xs)
--           | x1 + x2 < 3
--           = trace ("Can jump 2: " ++ show (c1, c2, c3, x1,x2,x3)) $
--             combs (c2+c1, c3+c1, 0) (x2:x3:xs) 
--           | otherwise
--           = trace ("Can jump 1: " ++ show (c1, c2, c3, x1,x2,x3)) $
--             combs (c2+c1, c3, 0)  (x2:x3:xs)


