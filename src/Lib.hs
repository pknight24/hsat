module Lib where


import Hsat.Algorithms.Backtracking
import Hsat.Models.Expression

exs = [Not $ Free "c", Const True, Free "asdf", And [Const True, Free "a"], Or [Const False, Free "b"]]
