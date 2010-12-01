module Main where

import System ( getArgs )
import Parser
import TypeCheck
import CodeGen

main :: IO Int
main = do
  [fi, fo] <- getArgs
  f <- readFile fi
  case maliceParser f fi of
    Left e   -> putStr ("Parse error:\n" ++ show e) >> error "Aborting."
    Right ast' -> case maliceTypeCheck ast' of
      Left tce -> putStr ("Type checking error:\n" ++ show tce) >> error "Aborting."
--      Right ast -> putStrLn (codeGenAST ast) >> return 0
      Right ast -> writeFile (fo ++ ".asm") (codeGenAST ast) >> return 0