module Main where

import System ( getArgs )
import CodeGen
import Semantics
import Parser
import Scanner

main
  = do [fn] <- getArgs
       f <- readFile fn
       let ast = maliceParser $ maliceScanner f
       putStrLn ("Abstract Syntax Tree:\n" ++ (show ast) ++ "\n")
       sem <- maliceSemantics ast
       if sem
         then putStrLn ("The semantics test was successful.\n\n" ++ "Generated C code:\n" ++ (convertProgramToC ast))
         else putStrLn "Semantics test failed."
