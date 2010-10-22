module Main where

import System ( getArgs )
import CodeGenC
import Parser
import Scanner

main = do 
  inStr <- getContents
  let code = show $ maliceParser $ maliceScanner inStr
  putStrLn code
--  putStrLn ("C code output:\n" ++ code)