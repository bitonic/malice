module CodeCleanup where

import Parser

sortDecls :: StatementList -> StatementList
sortDecls xs
  = [ Declare t x | Declare t x <- xs ] ++ removeDecls xs

removeDecls :: StatementList -> StatementList
removeDecls ((Declare t x) : xs)
  = removeDecls xs
removeDecls (x:xs)
  = x : (removeDecls xs)
removeDecls []
  = []