
import Data.List (sort)

-- import Data.List.Split (splitWhen) -- doesn't work on my system, so just rederive splitWhen
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = reverse $ map reverse $  f [[]] xs
  where f (r:rs) (y:ys) =
          if p y
          then f ([]:r:rs) ys
          else f ((y:r):rs) ys
        f rs [] = rs

paragraphs :: String -> [String]
paragraphs = map unwords . splitWhen null . lines

main = interact $ (++ "\n") . show . count good . map cannonical . paragraphs
  where cannonical = unwords . sort . map (head . splitWhen (== ':')) . words
        good x = (x == "byr cid ecl eyr hcl hgt iyr pid") || (x == "byr ecl eyr hcl hgt iyr pid")
        count p = length . filter p 
                                                         

