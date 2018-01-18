module Main where

import Site.Server        (runSite)
import Site.Static        (buildSite)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [root] -> do putStrLn "-------------------------------------------------------------"
                 putStrLn "  Launching storm-country.com on localhost:8000"
                 putStrLn "-------------------------------------------------------------"
                 runSite root
    [root, "--static", tgt] -> do
                 putStrLn ("Rendering static site to " ++ tgt)
                 buildSite root tgt
    _ -> putStrLn ("Expected the site root directory, got " ++ show args)

