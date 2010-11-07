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
       let parseResult = maliceParser f fn in
         case parseResult of
           Left e    -> return ()
           Right ast -> do putStrLn ("Abstract Syntax Tree:\n" ++ (show ast) ++ "\n")
                           putStrLn $ show $ llProgram $ unPosAST ast