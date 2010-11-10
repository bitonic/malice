module CodeCleanup where

import Parser

sortDecls :: StatementList -> StatementList
sortDecls xs
  = (getDecls xs) ++ (removeDecls xs)

getDecls :: StatementList -> StatementList
getDecls xs
  = [ Declare t x | Declare t x <- xs ]

removeDecls :: StatementList -> StatementList
removeDecls ((Declare t x) : xs)
  = removeDecls xs
removeDecls (x:xs)
  = x : (removeDecls xs)
removeDecls []
  = []