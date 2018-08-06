module Hsat.Boolish where

import Data.Set
  
class Boolish a where
  truthSet :: Set a
  falseSet :: Set a


instance Boolish Bool where
  truthSet = fromList [True]
  falseSet = fromList [False]

truth :: (Boolish a, Eq a) => a -> Bool
truth a = elem a truthSet

untruth :: (Boolish a) => Bool -> Set a
untruth b = 
  case b of
    True -> truthSet
    False -> falseSet
