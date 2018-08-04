module Hsat.Models.Result where

import qualified Data.Map as M

type ValueMap = M.Map String Bool

data Result = Success ValueMap | Failure deriving (Show, Eq)
