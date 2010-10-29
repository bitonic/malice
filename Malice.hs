module Main where

import System ( getArgs )
import Codegen
import Parser
import Scanner

main = do 
  [fn] <- getArgs
  f <- readFile fn
  let code = maliceParser $ maliceScanner f
  putStrLn $ show code
--  putStrLn $ convertProgramToC code
  putStrLn $ show (llProgram code)
