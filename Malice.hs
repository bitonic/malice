module Main where

import System ( getArgs )
import CodeGen
import Code2C
import Semantics
import Parser
import Scanner

main
  = do [fn] <- getArgs
       f <- readFile fn
       let parseResult = maliceParser f in
         case parseResult of
           Left e    -> return ()
           Right ast -> do putStrLn ("Abstract Syntax Tree:\n" ++ (show ast) ++ "\n")
                           sem <- maliceSemantics ast
                           if sem
                             then putStrLn $ show $ llProgram ast
                             else putStrLn "Semantics test failed."
