import Debug.Trace
import Data.List (sort, nub, intersect, foldl1)

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn test xs = reverse $ map reverse $ foldl f [[]] xs
  where f (l:ls) x = if test x then ([]:l:ls) else ((x:l):ls)
  
paragraphs = map unwords . splitOn null . lines

removeSpaces = filter (/= ' ')

main = interact $ (++ "\n") . show .
  sum . map (length . nub . foldl1 intersect . words) .
  tracef (sum . map (length . nub . removeSpaces)) . -- part 1
  paragraphs
