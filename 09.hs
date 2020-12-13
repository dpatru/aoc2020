import Debug.Trace (trace)
import qualified Data.Map as M

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

type Q = M.Map Int Int -- index -> number (This is a circular q.)
type Sums = M.Map Int Int -- sum -> expire index (If the current index is greater than the expire index, the sum is out-of-date.

-- main = interact $ show . check 25 . map read . lines
main = interact $ show . findWeakness . map read . lines

findWeakness :: [Int] -> Int
findWeakness xs = search (head xs) xs 1
  where goal = fst $ check 25 xs
        search v a l -- search for the goal, v is the current sum of the values from a of length l
          | v < goal
          = trace (show v ++ " is too small, ab = " ++ show (take (l+1) a)) $
            search (sum $ take (l+1) a) a (l+1)
          | v == goal
            = let ab = take l a
              in tracef (\g -> "found it! "++ show (take l a) ++ "\nsum = " ++ show v ++ "\nmax + min = " ++ show g) $
                 maximum ab + minimum ab
          | v > goal
          = trace (show v ++ " is too big") $
            search (v - head a) (tail a) (l-1)
          
check :: Int -> [Int] -> (Int, Int) 
check n = f 0 M.empty M.empty -- n: cycle length, 
  where
    f :: Int -> Q -> Sums -> [Int] -> (Int, Int)
    f _ _ _ [] = error "No violation found"
    f i q sums (x:xs)
    -- i: 0-based index of input values,
    -- q: circular queue (implemented as a map) holding the last n values of the input,
    -- sums: a map mapping sums to expiration (the last index value the sum may be used,
    -- (x:xs): input values
      | i < n -- filling up the queue
      = -- trace (show (i, x)) $ trace (show sums') $
          f (i+1) q' sums' xs
      | x `M.notMember` sums || i > (sums M.! x) -- found a value not in sums, stop
      = tracef fst $
          (x, i)
      | otherwise -- value found in sums, keep going.
      = -- trace (show (i, x)) $ trace (show sums') $
          f (i+1) q' sums' xs
      where q' = M.insert (i `mod` n) x q
            sums' = M.unionWith max
                    (M.fromList [(x', j+n)
                                | j <- [max 0 (i-n) .. max (-1) (i-1)]
                                , let x' = x + (q M.! (j `mod` n))])
                                       -- maybe 0 (+ x) ((j `mod` n) `M.lookup` q)])
                    sums

