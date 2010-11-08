module Semantics
       (
         maliceSemantics,
         SymbolTable
       )
       where

import Parser
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

-- The operators types
opTypes :: MaliceType -> [String]
opTypes Int32 = ["+", "-", "*", "/", "%", "^", "&", "|"]
opTypes Char8 = []

-- The symboltable
type SymbolTable = Map String MaliceType

-- The error type. SourcePos comes from Text.Parsec
type Message = String

data SemError = SemError !SourcePos Message

instance Error SemError where
  noMsg    = SemError (SourcePos "(unknown)" 0 0) "Unknown error."
  strMsg s = SemError (SourcePos "(unknown)" 0 0) s
  
instance Show SemError where
  show pos mess =
    show pos ++ "\n" ++ mess ++ "\n"
    
-- The monad, derived from either, to catch errors
type SemMonad = Either SemError

semantics :: ASTPos -> StateT (SymbolTable, SourcePos) SemMonad SymbolTable
semantics (ProgramPos sl) = do
  put (SourcePos "(unknown)" 0 0, M.empty)
  lift $ catchError (semSL sl) Left
  (st, _) <- get
  return st

semSL []       = return ()
semSL (s : sl) = semS s >> semSL sl

semS (pos, Assign v expr) = do
  st <- updatePos pos
  case (M.lookup v st) of
    Nothing -> lift $ throwError (SemError pos ("Trying to assign a value to the var \"" ++
                                                v ++ "\" which has not been declared."))
    Just tVar -> do {
      tExpr <- semExpr expr;
      if tExpr == tVar
        then return ()
        else lift $ throwError (SemError pos ("Trying to assign a value of type \"" ++
                                              show tExpr ++ "\" to variable \"" ++ v ++
                                              "\" of type \"" ++ show tVar ++ "\"."));
      }
                          
updatePos pos = do
  (st, oldPos) <- get
  put (st, pos)
  return st
                                
semExpr (Int _) _ = return Int32
semExpr (Char _) _ = return Char8
semExpr (Var v) = do 
  (st, _) <- get
  return (v M.! st)
semExpr (UnOp op e) = do
  (st, _) <- get
  t <- semExpr e st
  semOp op t
  return t
semExpr (BinOp op e1 e2) = do
  (st, pos) <- get
  t1 <- semExpr e1 st
  t2 <- semExpr e2 st
  semOp op t1
  if t1 == t2
    then return t1
    else lift $ throwError (SemError pos "Invalid use of operator \"" ++ op ++
                            "\" over types \"" ++ show t1 ++ "\" and \"" ++
                            show t2 ++ "\".")

                            
semOp op t
  | elem op (opTypes t)   = return ()
  | otherwise (opTypes t) =
    lift $ throwError ("The \"" ++ op ++ "\" operator does not support " ++
                       show t ++ " types.")
    