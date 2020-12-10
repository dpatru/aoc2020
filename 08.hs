{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
import Debug.Trace (trace)
import Data.Map (Map, fromList, (!), size, insert, findMax, notMember)
import qualified Data.Set as S
import Text.Regex.PCRE.Heavy (Regex, re, scan, split)

tracef :: Show b => (a->b) -> a -> a
tracef f x = trace (show $ f x) x

data Instruction = Nop Int -- need the int to enable toggling for part 2
                 | Acc Int
                 | Jmp Int
  deriving Show
  
type Program = Map Int Instruction

type Computer = (Program, Int, Int)

main = interact $
  (++ "\n")
  . show
  . search 0
  . tracef (\p -> "part 1: "++ (show $ runProgram p))
  . compileProgram

compileProgram :: String -> Program
compileProgram s =
  fromList
  $ [(i, fromStr cmd num)
    | (i, (_, (cmd: num: _))) <- zip [0 ..]
                                 $ scan [re|(\w+) \+?(-?\d+)|] s]
  where
    fromStr :: String -> String -> Instruction
    fromStr "nop" n = Nop $ read n
    fromStr "acc" n = Acc $ read n
    fromStr "jmp" n = Jmp $ read n
    fromStr code n = error $ "Bad instruction "++code++" "++n

runProgram :: Program -> Int
runProgram p = run1 0 0 S.empty
  where run1 pc acc seen =
          if pc `S.member` seen then acc
          else let seen' = pc `S.insert` seen in
            case p!pc of
              Nop _ -> run1 (pc + 1) acc seen'
              Acc n -> run1 (pc + 1) (acc + n) seen'
              Jmp n -> run1 (pc + n) acc seen'
              -- _ -> error $ "Bad instruction: " ++ show (p!pc)

search :: Int -> Program -> Int
search n p = -- n = line of program modification, p = program
  trace ("Trying " ++ show n) $
  if n > lastAddr then error "Failed search"
  else case p!n of
         Acc _ -> search (n+1) p -- Acc don't change, skip
         Nop x -> run1 0 0 S.empty (insert n (Jmp x) p)
         Jmp x -> run1 0 0 S.empty (insert n (Nop x) p)
  where
    lastAddr :: Int
    lastAddr = fst $ findMax p
    run1 :: Int -> Int -> S.Set Int -> Program -> Int
    run1 pc acc seen p' =
      if pc == lastAddr + 1 then acc
      else if pc `S.member` seen || pc `notMember` p' then search (n+1) p
           else let seen' = pc `S.insert` seen
                in case tracef (\i->(pc, i)) (p'!pc) of
                     Nop _ -> run1 (pc + 1) acc seen' p'
                     Acc x -> run1 (pc + 1) (acc + x) seen' p'
                     Jmp x -> run1 (pc + x) acc seen' p'
              
