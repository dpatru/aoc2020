import Data.List (foldl, break, (!!))
import Debug.Trace (trace)

countTrues :: [Bool] -> Int
countTrues bools = foldl (\b a -> if a then b+1 else b) 0 bools

count :: Eq a => a -> [a] -> Int
count x ys = foldl (\b a-> if a == x then b+1 else b) 0 ys

verifyPassword :: String -> Bool
verifyPassword l = trace
                   ("minbound: " ++ show (read minBound :: Int) ++ ", maxBound: " ++ show (read maxBound :: Int) ++ ", letter: " ++ show letter ++ ", count: " ++ show c ++ ", input: " ++ l ++ ", pass: " ++ show pass) $ pass
  where (minBound, '-':maxBound) = break (== '-') bounds
        (bounds, ' ':letter:[]) = break (== ' ') rule
        (rule, ':':' ':passwd) = break (== ':') l
        c = count letter passwd
        pass = c >= read minBound && c <= read maxBound

-- main = interact $ (++ "\n") . show . countTrues . map verifyPassword . lines

xor :: Bool -> Bool -> Bool
xor a b = if a then not b else b

verifyPassword2 :: String -> Bool
verifyPassword2 l = trace
                   ("p1: " ++ show p1 ++ "p2: " ++ show p2 ++ ", letter: " ++ show letter ++ ", input: " ++ l ++ ", pass: " ++ show pass) $ pass
  where (p1String, '-':p2String) = break (== '-') positions
        p1 = read p1String - 1
        p2 = read p2String - 1
        (positions, ' ':letter:[]) = break (== ' ') rule
        (rule, ':':' ':passwd) = break (== ':') l
        pass = (passwd!!p1 == letter) `xor` (passwd!!p2 == letter)
        

main = interact $ (++ "\n") . show . countTrues . map verifyPassword2 . lines
  
