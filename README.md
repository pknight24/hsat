# hsat

*hsat* is a SAT Solver, written completely in Haskell. Currently, the only algorithm in use is a brute-force backtracking method. I designed the Module system with the intent of adding more efficient algorithms in the future.

Additionally, the only way the interact with *hsat* is by running 

> stack ghci

and passing in Expressions manually. I plan on written a small DSL to make this process easier.
