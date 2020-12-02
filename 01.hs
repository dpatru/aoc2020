import Debug.Trace (trace, traceShow, traceShowId)
import Data.List (sort)

(&&&) f g x = (f x, g x) -- apply an argument to two functions

tracef f x = traceShow (f x) x

-- main = interact $ (++ "\n") . show . (find2 2020 &&& find3 2020) . sort . map read . lines
main = interact $ (++ "\n") . show . find3 2020 . sort . map read . lines


find2 :: Int -> [Int] -> Maybe (Int, Int, Int)
find2 n (x:xs) = if x' `elem` xs then return (x, x', x * x') else find2 n xs
  where x' = n - x
find2 _ [] = Nothing

find3 n (x:xs) = maybe (find3 n xs)
                       (\(y,z,_) -> return (x,y,z,x*y*z)) $
                   find2 (n-x) xs
find3 _ [] = Nothing
