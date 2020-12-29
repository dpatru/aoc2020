import Debug.Trace
import Data.List (sort, tails)

traceIdShow :: Show a => a -> a
traceIdShow x = trace (show x) x

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x [] = []
splitOn x xs = y: splitOn x ys
  where y = takeWhile (/= x) xs
        ys = safeTail $ dropWhile (/= x) xs
        safeTail [] = []
        safeTail (z:zs) = zs

allPairs :: [a] -> [(a,a)]
allPairs ls = [(x,y) | x:xs <- tails ls, y <- xs]

relativelyPrime :: Integral a => [a] -> Bool
relativelyPrime = tracef (\v->"Relatively Prime: " ++ show v)
  . all (\(x,y) -> gcd x y == 1) . allPairs

main = interact $ (++ "\n") .  show
  . part2
  . tracef (relativelyPrime . map snd) -- check for prime
  . snd -- keep just the bus ids
  . tracef part1
  . (\(time: busses:_)->
        ((read time :: Int)
        , [(i, read x :: Int) | (i, x) <- zip [0..] $ splitOn ',' busses
                              , x /= "x"]))
  . tracef (\ls -> "lines are: " ++ (unlines ls))
  . lines

part1 :: (Int, [(Int,Int)]) -> String -- [Int]Float -> [Float] -> [Float] -- (Float, Float, Float, Float)
part1 (t, xs) =
  "Part 1: (wait time, busId, waitTime * busId) = "
  ++ (show 
      $ minimum [(w, x, w * x)
                | (_,x) <- xs
                , let w = t `div` x * x + x - t])

part2 :: [(Int,Int)] -> Integer
part2 ((_,x):ps) = f x' x' [(toInteger i, toInteger y) | (i,y) <- ps]
  where
    x' = toInteger x
    f :: Integer -> Integer -> [(Integer,Integer)] -> Integer
    -- f n increment remainingMatches: checks if n satifies the next
    -- bus. (n satisfies a bus if the remainder of n divided by bus is
    -- i, the bus's index) If it does, it takes that bus off the
    -- remainingMatches list and updates the increment to ensure that
    -- the increment is divisible by the bus just matched. This
    -- ensures that incrementing n will not change n `mod` bus. If n
    -- does not satisfy the next match, increment n.
    f n increment [] = trace ("Part2: " ++ show n) n
    f n increment ((i,y):ys)
      | (n+i) `mod` y == 0
      -- found a match, make the increment a multiple of y, so that
      -- additional increments added to n will not change n `mod` y.
      = f n increment' ys
      | otherwise 
      = f (n+increment) increment ((i,y):ys)
      where increment' = (increment * y) `div` (gcd increment y)
            -- if increment and y share common factors, then dividing
            -- by the gcd removes the duplicate factors. In my
            -- problem, this was not an issue, simply setting the new
            -- increment to the old increment * y gives the same
            -- answer.

