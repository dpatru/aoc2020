main = interact $ (++ "\n") . show . find2020 . map read . lines

find2020 (x:xs) = if x' `elem` xs then x * x' else find2020 xs
  where x' = 2020 - x
find2020 [] = -1
