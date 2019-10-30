module Main where

import MinimalExample
import ServantSyntax

main :: IO ()
main =
  runStartApp
  -- runDiffMonad

runStartApp :: IO ()
runStartApp = do
  putStrLn "Running startApp"
  startApp

runDiffMonad :: IO ()
runDiffMonad = do
  putStrLn "Running differentMonadServer"
  webAppUsingDifferentMonad

{-

-- for 'runStartApp'
curl http://localhost:8080/

-- for 'runDiffMonad'
curl http://localhost:8080/simpleRoute
-}
