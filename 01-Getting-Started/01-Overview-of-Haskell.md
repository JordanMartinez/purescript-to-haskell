# Overview of Haskell

Cover these things
- Compiler
  - GHC - The state-of-the-art Haskell compiler
  - GHCi - GHC's REPL. Note: this is far more powerful than PureScript's REPL
- Documentation
  - Hackage - Haskell's Pursuit, but without the search engine
  - Hoogle - Search engine for Hackage (e.g. Pursuit's search bar)
  - Stackage - What would occur if Spago created an instance of Pursuit that only had documentation from repositories in a given package set.
- Build tools
  - Cabal - The main build tool of Haskell
  - Stack - Similar to Spago in that it uses something like a package set but requires you to modify a file to add additional dependencies not in the set. However, it's goal is to be a build tool (e.g. `pulp` or the `spago build` part of spago), not a dependency manager (e.g. the `spago install halogen` part of spago).
- Dependency Manager
  - Cabal ?
  - Nix ?
