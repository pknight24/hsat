module Hsat.Models.Result where

import qualified Data.Map as M

type ValueMap a = M.Map String a

data Result a = Success (ValueMap a) | Failure deriving (Show, Eq)
