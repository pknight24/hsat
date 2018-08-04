module Hsat.Models.Expression where

data Expression =
            And [Expression]
            | Or [Expression]
            | Not Expression
            | Free String
            | Const Bool
            deriving (Show, Eq)

replace :: String 
           -> Bool 
           -> Expression 
           -> Expression
replace v b ex = 
  case ex of
    (Free s) -> if s == v then Const b else Free s
    (And exs) -> And $ replaceAll v b exs
    (Or exs) -> Or $ replaceAll v b exs
    Not e -> Not $ replace v b e
    _ -> ex

replaceAll :: String -> Bool -> [Expression] -> [Expression]
replaceAll v b exs = fmap (replace v b) exs

simplify :: Expression -> Expression
simplify ex = 
  case ex of
    (Const b) -> Const b
    (Free s) -> Free s
    (Not e) -> 
      case simplify e of
        (Const bb) -> Const $ not bb
        _ -> Not $ simplify e
    (Or exs) -> case elem (Const True) exs of
                True -> Const True
                False -> Or $ fmap simplify $ 
                                 filter (\ex -> ex /= (Const False)) exs
    (And exs) -> case elem (Const False) exs of
                   True -> Const False
                   False -> And $ fmap simplify $
                                     filter (\ex -> ex /= (Const True)) exs
  
satisfiable :: Expression -> Maybe Bool
satisfiable ex = 
  case ex of
    (Const b) -> Just b
    (Free s) -> Nothing
    (Not e) -> case satisfiable e of
                 Nothing -> Nothing
                 Just b -> Just $ not b
    (Or exs) -> do
                  checked <- mapM satisfiable exs
                  pure $ foldl1 (||) checked
    (And exs) -> do
                  checked <-  mapM satisfiable exs
                  pure $ foldl1 (&&) checked


freeVars :: Expression -> [String]
freeVars ex = 
  case ex of
    (Const b) -> []
    (Free s) -> [s]
    (Not e) -> freeVars e
    (And exs) -> concat $ fmap freeVars exs
    (Or exs) -> concat $ fmap freeVars exs

