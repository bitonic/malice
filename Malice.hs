module Main where

import System ( getArgs )
import System.FilePath.Posix ( takeBaseName, dropExtension )
import System.Process ( runProcess, waitForProcess )
import CodeGen
import Semantics
import Parser
import Reduce

main = do
  [fn] <- getArgs
  f <- readFile fn
  case maliceParser f fn of
    Left e    -> putStr ("Parse error:\n" ++ show e)
    Right ast -> case maliceSemantics ast of
      Left e   -> putStr ("Semantics error:\n" ++ show e)
      Right st -> let bf = dropExtension $ takeBaseName fn in do {
        writeFile (bf ++ ".asm") (codeGen $ reduceAST $ unPosAST ast);
        runProcess "./compileasm" [bf] Nothing Nothing Nothing Nothing Nothing;
        return (); }
                                            
                             