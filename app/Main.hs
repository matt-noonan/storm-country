module Main where

import Site.Server        (runSite)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [root] -> do putStrLn "-------------------------------------------------------------"
                 putStrLn "  Launching storm-country.com on localhost:8000"
                 putStrLn "-------------------------------------------------------------"
                 runSite root
    _ -> putStrLn ("Expected the site root directory, got " ++ show args)

