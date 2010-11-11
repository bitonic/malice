module Main where

import System ( getArgs )
import CodeGen
import Semantics
import Parser

main :: IO Int
main = do
  [fi, fo] <- getArgs
  f <- readFile fi
  case maliceParser f fi of
    Left e   -> putStr ("Parse error:\n" ++ show e) >> return 1
    Right sl -> case maliceSemantics sl of
      Left e   -> putStr ("Semantics error:\n" ++ show e) >> return 1
      Right st -> writeFile (fo ++ ".asm") (codeGen (unPosSL sl) st) >> return 0
                                            
                             