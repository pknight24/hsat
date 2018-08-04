module Hsat.Algorithms.Backtracking (solve) where

import qualified Data.Map as M

import Hsat.Models.Expression
import Hsat.Models.Result

totallySat :: [Expression] -> Bool
totallySat exs = all (\a -> a == (Just True)) $ 
                   fmap satisfiable exs

makeAGuess :: String -> Bool -> [Expression] -> ValueMap -> ([Expression], ValueMap)
makeAGuess s v exs vm = (replaceAll s v exs, M.insert s v vm)

backtrack :: [Expression] -> ValueMap ->Result
backtrack exs state = 
  case concat $ fmap freeVars exs of
    [] -> case totallySat exs of
            True -> Success state
            False -> Failure
    (v:vs) -> let trueGuess = makeAGuess v True exs state in
              case backtrack (fst trueGuess) (snd trueGuess) of
                Failure -> let falseGuess = makeAGuess v False exs state in
                             backtrack (fst falseGuess) (snd falseGuess)
                success -> success
              


solve :: [Expression] -> Result
solve exs = 
 case elem (Just False) $ fmap satisfiable exs of
 True -> Failure -- the expressions can't be satisfied
 False -> backtrack exs $ M.empty
          
