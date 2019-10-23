# Haskell REPL

## Quick Broad Overview

Assuming your current working directory is still `~/Haskell-project`, run `stack ghci` to start the repl.

Once started, here's a quick overview of some of the things you can do to make the experience better. Once done here, skim through the [GHCi User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html):
```
-- Change the prompt, so that it doesn't show all modules that are currently imported.
*Main Lib> :set prompt "> "
> -- now the promp is '> '

-- Print a number
> 4
4

-- Use `:{` to start a multi-line expression
-- and end it with :}
> :{
*Main Lib| foo :: Int
*Main Lib| foo = 4
*Main Lib| :}
> foo
4

-- Fix the "*Main Lib| " prompt
> :set prompt-cont "| "
> :{
| foo :: Int
| foo = 4
| :}
> foo
4

-- Also print the type of the output
> :set +t -- use `:unset +t` to unset
> 4
4
it :: Num p => p

-- Show dependencies whose modules can be imported.
-- If your `packages.yml` file includes
-- additional dependencies, they'll
-- show up here.
> :show linker
----- Linker state -----
Pkgs: [base-4.12.0.0, integer-gmp-1.0.2.0, ghc-prim-0.5.3, rts-1.0]
Objs: []
BCOs: []

-- Show modules that are currently imported
> :show imports
import Lib
:module +*Main -- added automatically

-- import a module, whether from your library or from a dependency
> import Data.Int
> :show imports
import Lib
import Data.Int
:module +*Main -- added automatically

-- unimport a module
> :module - Data.Text
> :show imports
import Lib
:module +*Main -- added automatically

-- Browse a given module
> :browse Lib
someFunc :: IO ()

-- Browse a given module and show more detail
-- The below example isn't a good one unfortunately.
> :browse! Lib
someFunc :: IO ()

-- Show the source code for an identifier
> :list someFunc
5  someFunc :: IO ()
6  someFunc = putStrLn "someFunc"
7

-- Try calling `:browse Prelude`

-- Get the type of an expression
> :type [1, 2 ,3]
[1, 2 ,3] :: Num a => [a]

-- Get the fully evaluated type of an expression...?
> :type +d [1, 2 ,3]
[1, 2 ,3] :: [Integer]

-- Get the documentation for an entity
> :doc words
 'words' breaks a string up into a list of words, which were delimited
 by white space.

 >>> words "Lorem ipsum\ndolor"
 ["Lorem","ipsum","dolor"]

-- Enable a language extension
> :set -XOverloadedStrings

-- Enable a GHC option
> :set -Wall

-- Quit the REPL
> :quit

-- Now all of the settings you set and bindings you made are gone.

-- Restarting the GHCi...
stack ghci
-- output...

-- The prompt is back to what it was previously.
*Main Lib>
```
To load the GHCi with your default settings, such as `:set +t` or `:set prompt "> "`, create a file in your Haskell project called `.ghci` and list each setting on a new line.

An example `.ghci` file might look like this:
```
:set +t
:set prompt "> "
:set prompt-cont "> "
:set -Wall
:set -XOverloadedStrings
```

## Scoping Rules in GHCi

Read through [What's realy in scope at the prompt?](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#what-s-really-in-scope-at-the-prompt)
