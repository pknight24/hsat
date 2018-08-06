module Hsat.Algorithms.Backtracking (solve) where

import qualified Data.Map as M
import qualified Data.Set as S

import Hsat.Models.Expression
import Hsat.Models.Result
import Hsat.Boolish

totallySat :: (Boolish a, Eq a) => [Expression a] -> Bool
totallySat exs = all (\a -> a == (Just True)) $ 
                   fmap satisfiable exs

makeAGuess :: (Boolish a) => 
              String -> 
              a ->
              [Expression a] -> 
              ValueMap a -> 
              ([Expression a], ValueMap a)
makeAGuess s v exs vm = (replaceAll s v exs, M.insert s v vm)

backtrack :: [Expression Bool] -> ValueMap Bool -> Result Bool
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
              


solve :: (Boolish a, Eq a) => [Expression a] -> Result (S.Set a)
solve exs = 
 case elem (Just False) $ fmap satisfiable exs of
 True -> Failure -- the expressions can't be satisfied
 False -> let result = backtrack (fmap (truth <$>) exs) M.empty in
          case result of
            Failure -> Failure
            Success s -> Success $ fmap untruth s
