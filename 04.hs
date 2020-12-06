import Debug.Trace (trace)
import Data.List (sort, takeWhile, dropWhile)

tracef :: Show b => (a -> b) -> a -> a
tracef f x = trace (show (f x)) x

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

isLowletter x = x `elem` ['a'..'z']
isDigit x = x `elem` ['0'..'9']
isNumber x = all isDigit x

parseHeight x = (read h :: Int, unit)
  where h = takeWhile isDigit x
        unit = dropWhile isDigit x
        
main = interact $
  (++ "\n") . show . length . 
  tracef length . filter valid . map (map (splitWhen (== ':')) . words) .
  tracef length . filter present . map cannonical .
  paragraphs
  where cannonical = unwords . sort . words
        present  =
          (\y -> (y == "byr cid ecl eyr hcl hgt iyr pid") ||
                 (y == "byr ecl eyr hcl hgt iyr pid")) .
          unwords . map (head . splitWhen (== ':')) . words
        count p = length . filter p 
        valid = all validField
        validField (field:value:[])
          -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
          | field == "byr" = let y = read value in 
              1920 <= y && y <= 2002
          -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
          | field == "iyr" = let y = read value in
              2010 <= y && y <= 2020
          -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
          | field == "eyr" = let y = read value in
              2020 <= y && y <= 2030
          -- hgt (Height) - a number followed by either cm or in:
          -- If cm, the number must be at least 150 and at most 193.
          -- If in, the number must be at least 59 and at most 76.
          | field == "hgt" = let (h, units) = parseHeight value in
              ((units == "cm" && 150 <= h && h <= 193) ||
               (units == "in" && 59 <= h && h <= 76))
          -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
          | field == "hcl" = let (hash:rest) = value in 
            hash == '#' && ((== 6) $ length $ filter (\c -> isLowletter c || isDigit c) rest)
          -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
          | field == "ecl" =
            value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          -- pid (Passport ID) - a nine-digit number, including leading zeroes.
          | field == "pid" =
            all isDigit value && length value == 9
          -- cid (Country ID) - ignored, missing or not.
          | field == "cid" = True
          | otherwise = False
          


