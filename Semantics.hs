module Semantics
       (
         maliceSemantics,
         SymbolTable
       )
       where

import Parser
import Text.ParserCombinators.Parsec.Pos (SourcePos(..), newPos)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

-- The operators types
opTypes :: MaliceType -> [String]
opTypes MaliceInt = ["+", "-", "*", "/", "%", "^", "&", "|"]
opTypes MaliceChar = []

-- The symboltable
type SymbolTable = Map String MaliceType

-- The error type. SourcePos comes from Text.Parsec
type Message = String

data SemError = SemError !SourcePos Message

instance Error SemError where
  noMsg    = SemError (newPos "(unknown)" 0 0) "Unknown error."
  strMsg s = SemError (newPos "(unknown)" 0 0) s

instance Show SemError where
  show (SemError pos mess) =
    show pos ++ "\n" ++ mess ++ "\n"

-- The monad, derived from either, to catch errors
type SemMonad = ErrorT SemError (State (SymbolTable, SourcePos))

semantics :: ASTPos -> SemMonad (Either SemError SymbolTable)
semantics (ProgramPos sl) = do
  put (M.empty, newPos "(unknown)" 0 0)
  catchError (semSL sl) (return . Left)
  (st, _) <- get
  return (Right st)

semSL (s : sl) = semS s >> semSL sl

semS (pos, Assign v expr) = do
  st <- updatePos pos
  case (M.lookup v st) of
    Nothing -> throwError (
      SemError pos ("Trying to assign a value to the var \"" ++
                    v ++ "\" which has not been declared."))
    Just tVar -> do {
      tExpr <- semExpr expr;
      if tExpr == tVar
        then return ()
        else throwError (SemError pos ("Trying to assign a value of type \"" ++
                                       show tExpr ++ "\" to variable \"" ++ v ++
                                       "\" of type \"" ++ show tVar ++ "\"."));
      }
                          
updatePos pos = do
  (st, oldPos) <- get
  put (st, pos)
  return st
                                
semExpr :: Expr -> SemMonad MaliceType
semExpr (Int _) = return MaliceInt
semExpr (Char _) = return MaliceChar
semExpr (Var v) = do 
  (st, _) <- get
  return (st M.! v)
semExpr (UnOp op e) = do
  t <- semExpr e
  semOp op t
  return t
semExpr (BinOp op e1 e2) = do
  (_, pos) <- get
  t1 <- semExpr e1
  t2 <- semExpr e2
  semOp op t1
  if t1 == t2
    then return t1
    else throwError (SemError pos ("Invalid use of operator \"" ++ op ++
                                   "\" over types \"" ++ show t1 ++ "\" and \"" ++
                                   show t2 ++ "\"."))

                            
semOp op t
  | elem op (opTypes t) = return ()
  | otherwise = throwError ("The \"" ++ op ++ "\" operator does not support " ++
                            show t ++ " types.")
    