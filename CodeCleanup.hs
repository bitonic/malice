module CodeCleanup where

import Parser



sortDecls :: StatementList -> StatementList
sortDecls xs
  = [ Declare x | Declare x <- xs ] ++ removeDecls xs

removeDecls :: StatementList -> StatementList
removeDecls ((Declare x) : xs)
  = removeDecls xs
removeDecls (x:xs)
  = x : (removeDecls xs)
removeDecls []
  = []
