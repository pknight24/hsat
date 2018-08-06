# hsat

*hsat* is a SAT Solver, written completely in Haskell. Currently, the only algorithm in use is a brute-force backtracking method. I designed the Module system with the intent of adding more efficient algorithms in the future.

The goal is to make satisfiability problems more generalizable by leverage Haskell's type system. You can build expressions of **Boolish** types, and pass them to the solver. The solver then returns either a Failure message, or a map of free variables to the set of values that would satisfy the constraints. More on this to come.
