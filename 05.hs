import Debug.Trace (trace)
import Data.List (foldl', maximum, sort)

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

ticketToInt :: String -> Int
ticketToInt = foldl' (\acc x -> acc * 2 + if isOne x then 1 else 0) 0
  where isOne = (`elem` "BR") -- decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

intToTicket :: Int -> String
intToTicket x = f 512 x
  where f :: Int -> Int -> String
        f 0 _ = []
        f b y | b <= y && b > 8 = 'B':f b' (y-b)
              | b <= y && b <= 8 = 'R':f b' (y-b)
              | b >= 8 = 'F':f b' y
              | b < 8 = 'L':f b' y
          where b' = b `div` 2
              
main = interact $
  (++ "\n") . tracef ticketToInt . intToTicket . missing .
  -- tracef (map (\n -> (n, intToTicket n))) .
  sort .
  tracef maximum .
  map ticketToInt . words
  where missing (x:y:xs) | x+2 == y =
                           trace ("missing " ++ show x ++ ", " ++ show y) (x+1)
                         | null xs = error $ "none missing at end " ++ show x
                         | x+1 == y = missing (y:xs)
                         | otherwise = error $ "error missing at " ++ show x

