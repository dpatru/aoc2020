{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
import Debug.Trace (trace)
import Text.Regex.PCRE.Heavy (Regex, re, scan, split)
import Data.Map.Strict (Map, (!), empty, insert, insertWith, toList, foldrWithKey, member, unionWith, fromList)
import qualified Data.Set as S

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

reGroups :: Regex -> String -> [[String]]
reGroups re s = map snd $ scan re s

type Rule = Map String [(String, Int)]
parseRules :: String -> Rule
parseRules = foldl f empty . reGroups [re|(.*?) bags contain (.+?)\.|]
  where f:: Rule -> [String] -> Rule
        f rules (b:bs:_) = -- trace (show bs) $
          insert b (parseBags bs) rules
        parseBags :: String -> [(String, Int)]
        parseBags bs | bs == "no other bags" = []
                     | otherwise = -- tracef id .
                       map ((\(i:b:_) -> (b, read i))
                            . head . reGroups [re|(\d+) (.+?) bags?|])
                       $ split [re|, |] bs 

type IRule = Map String [String]
invertRules :: Rule -> IRule
invertRules = foldrWithKey addBags empty
  where addBags bag bs irules =
          unionWith (++) irules $ fromList [(b, [bag]) | (b, _) <- bs]
          -- foldr (\(b, _) irules'-> insertWith (++) b [bag] irules')
          -- irules bs

type ContainingSet = S.Set String
buildContaining :: String -> IRule -> ContainingSet
buildContaining k r = f S.empty $ (r!k)
  where f s' [] = s'
        f s' (x:xs) = if x `S.member` s' then f s' xs
                      else f (x `S.insert` s') $
                           if x `member` r then xs ++ r!x else xs

countBags :: String -> Rule -> Int
countBags k r = f (r!k)
  where f [] = 0
        f ((bag, n):bs) = n + n * f (r!bag) + f bs

main = interact $ (++ "\n") . show . countBags "shiny gold"
  . tracef (S.size . buildContaining "shiny gold" . invertRules)
  . parseRules


