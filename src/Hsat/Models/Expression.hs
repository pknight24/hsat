module Hsat.Models.Expression where

import Hsat.Boolish

data Expression a =
            And [Expression a]
            | Or [Expression a]
            | Not (Expression a)
            | Free String
            | Const a
            deriving (Show, Eq)

instance Functor Expression where
  fmap f ex = 
    case ex of
      Free s -> (Free s)
      Const e -> Const $ f e
      Not e -> Not $ fmap f e
      And exs -> And $ fmap (f <$>) exs     
      Or exs -> Or $ fmap (f <$>) exs

replace :: (Boolish a) => String 
           -> a 
           -> Expression a
           -> Expression a
replace v a ex = 
  case ex of
    (Free s) -> if s == v then Const a else Free s
    (And exs) -> And $ replaceAll v a exs
    (Or exs) -> Or $ replaceAll v a exs
    Not e -> Not $ replace v a e
    _ -> ex

replaceAll :: (Boolish a) => String -> a -> [Expression a] -> [Expression a]
replaceAll v a exs = fmap (replace v a) exs

satisfiable :: (Boolish a, Eq a) => Expression a -> Maybe Bool
satisfiable ex = 
  case ex of
    (Const b) -> Just $ truth b
    (Free s) -> Nothing
    (Not e) -> case satisfiable e of
                 Nothing -> Nothing
                 Just b -> Just . not $ truth b
    (Or exs) -> do
                  checked <- mapM satisfiable exs
                  pure $ or checked
    (And exs) -> do
                  checked <-  mapM satisfiable exs
                  pure $ and checked


freeVars :: Expression a -> [String]
freeVars ex = 
  case ex of
    (Const b) -> []
    (Free s) -> [s]
    (Not e) -> freeVars e
    (And exs) -> concat $ fmap freeVars exs
    (Or exs) -> concat $ fmap freeVars exs

