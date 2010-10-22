module Main where

import System ( getArgs )
import Parser
import Scanner

main = do 
  [fn] <- getArgs
  f <- readFile fn
  let code = show $ maliceParser $ maliceScanner f
  putStrLn code