import Debug.Trace
import Data.Map.Strict as M ((!), insertWith, fromList, Map, empty, mapWithKey, lookup, foldl)
import qualified Data.List as L (sort, map)
import Control.Monad (join)
-- import Data.VectorSpace ((^+^)) -- not found

(x,y) ^+^ (i,j) = (x+i, y+j)

m !? x = M.lookup x m

traceIdShow :: Show a => a -> a
traceIdShow x = trace (show x) x

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x [] = []
splitOn x (y:xs) | y == x = []: splitOn x xs
                 | otherwise = (y: ys) : yss
  where (ys: yss) = splitOn x xs


main = interact $ show . part2 . part1
       . buildMap
       . lines

data SeatMap = Seats Int Int (Map (Int,Int) Char)

instance Show SeatMap where
  show = (++ "\n") . showMap

instance Eq SeatMap where
  Seats x y m == Seats x' y' m' = x == x' && y == y' && m == m'
  
buildMap :: [String] -> SeatMap
buildMap ls = Seats 
  (length (head ls))
  (length ls)
  (fromList [((i,j), c) | (j, l) <- zip [0 ..] ls
                        , (i, c) <- zip [0 ..] l])

       
showMap :: SeatMap -> String
showMap (Seats x y m) = unlines [[m!(i,j) | i<-[0..x-1]] | j<-[0..y-1]]

applyRules :: SeatMap -> SeatMap
applyRules (Seats x y m) = Seats x y (mapWithKey rules m)
  where
    rules :: (Int,Int) -> Char -> Char
    rules (i,j) v 
      | v == 'L' && adj == 0 = '#'
      | v == '#' && adj >= 4 = 'L'
      | otherwise = v
      where adj = length [(i',j') | i'<-[i-1..i+1]
                                  , j'<-[j-1..j+1]
                                  , (i',j') /= (i,j)
                                  , (i',j') `M.lookup` m == Just '#']

applyRules2 :: SeatMap -> SeatMap
applyRules2 (Seats x y m) = Seats x y (mapWithKey rules m)
  where
    directions = [(dx,dy) | dx <- [-1,0,1]
                          , dy <- [-1,0,1]
                          , (dx,dy) /= (0,0)]
    neighbors = mapWithKey findNeighbors m
      where findNeighbors p _ = join [findNeighbor d p | d <- directions]
            findNeighbor d p | v == Nothing = []
                             | v == Just '.' = findNeighbor d p'
                             | otherwise = [p']
              where p' = p ^+^ d
                    v = m !? p'
    rules :: (Int,Int) -> Char -> Char
    rules (i,j) v 
      | v == 'L' && adj == 0 = '#'
      | v == '#' && adj >= 5 = 'L'
      | otherwise = v
      where adj = length [1 | n <- neighbors M.! (i,j)
                            , m M.! n == '#']

applyUntilStable :: Eq a => (a -> a) -> a -> a
applyUntilStable f x = run 0 x
  where run i y | y == y' = y
                | otherwise = trace (show i) $ run (i+1) y'
          where y' = f y
                
-- applyRulesUntilStable :: SeatMap -> SeatMap
-- applyRulesUntilStable m = run 0 m
--   where run i m | m == m' = trace ("done:\n"++ show m) m
--                 | otherwise = trace (show i) $ run (i+1) m'
--           where m' = applyRules m

countSeats :: SeatMap -> Int
countSeats (Seats _ _ m) = M.foldl (\s v-> if v == '#' then s+1 else s) 0 m

part1 :: SeatMap -> SeatMap
part1 = tracef (countSeats . applyUntilStable applyRules)

part2 :: SeatMap -> SeatMap
part2 m = trace ("part 2: " ++ show s) m'
  where m' = applyUntilStable applyRules2 m
        s = countSeats m'
