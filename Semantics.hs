module Semantics
       (
         maliceSemantics,
         VarTypes
       )
       where

import Parser
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

-- The operators types
opTypes MaliceInt = ["+", "-", "*", "/", "%", "^", "&", "|", "~"]
opTypes MaliceChar = []

-- The symboltable
type VarTypes = Map String MaliceType

-- The error type. SourcePos comes from Text.Parsec
type Message = String
data SemError = SemError !SourcePos Message

instance Error SemError where
  noMsg  = SemError (newPos "(unknown)" 0 0) "Unknown error."
  strMsg = SemError (newPos "(unknown)" 0 0)

instance Show SemError where
  show (SemError pos mess) =
    show pos ++ "\n" ++ mess ++ "\n"

-- The monad, derived from the error monad transformer.
-- The inner monad is a state with the current symbol table and
-- the current position. Probably it's possible to have a cleaner
-- Code using Applicative, but we did not have time to do that
-- for milestone 2.
type SemMonad = ErrorT SemError (State (VarTypes, SourcePos))

-- Helper function to throw errors
throwSemError s = do
  (_, pos) <- get
  throwError (SemError pos s)

-- The actual semantics analysis
semantics sl = do
  semSL sl
  (st, _) <- get
  return st

semSL :: StatementListPos -> SemMonad ()
semSL [] = throwSemError "Missing return statement."
semSL ((_, Return e) : _) = semExpr e >> return ()
semSL (s : sl) = semS s >> semSL sl

semS :: (SourcePos, Statement) -> SemMonad ()
semS (pos, Assign v expr) = do
  updatePos pos
  checkDecl v (
    \tVar -> do {
      tExpr <- semExpr expr;
      unless (tExpr == tVar) $ throwSemError (
        "Trying to assign a value of type \"" ++ show tExpr ++
        "\" to variable \"" ++ v ++ "\" of type \"" ++ show tVar ++ "\".");
      })
semS (pos, Declare t v) = do
  st <- updatePos pos
  case M.lookup v st of
    Nothing -> put (M.insert v t st, pos)
    _       -> throwSemError ("The variable \"" ++ v ++ "\" was already" ++
                              " declared.")
semS (pos, Decrease v) =
  updatePos pos
  checkDecl v (
    \t -> case t of
      MaliceInt -> return ()
      _         -> throwSemError ("Trying to decrease var \"" ++ v ++
                                  " of type \"" ++ show t ++ "\"."))
semS (pos, Increase v) =
  updatePos pos
  checkDecl v (
    \t -> case t of
      MaliceInt -> return ()
      _         -> throwSemError ("Trying to increase var \"" ++ v ++
                                  " of type \"" ++ show t ++ "\"."))
semS (_, Return _) = error "The function semS is to be called by semSL."

checkDecl :: String -> (MaliceType -> SemMonad a) -> SemMonad a
checkDecl v f = do
  (st, _) <- get
  case M.lookup v st of
    Nothing  -> throwSemError ("Trying to assign a value to the var \"" ++
                               v ++ "\" which has not been declared.")
    (Just t) -> f t

updatePos :: SourcePos -> SemMonad VarTypes 
updatePos pos = do
  (st, _) <- get
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
  t1 <- semExpr e1
  t2 <- semExpr e2
  semOp op t1
  if t1 == t2
    then return t1
    else throwSemError ("Invalid use of operator \"" ++ op ++
                        "\" over types \"" ++ show t1 ++ "\" and \"" ++
                        show t2 ++ "\".")

semOp :: String -> MaliceType -> SemMonad ()
semOp op t
  | op `elem` opTypes t = return ()
  | otherwise = do
    throwSemError ("The \"" ++ op ++ "\" operator does not support " ++
                   show t ++ " types.")
      
-- Semantics analysis from the ast with positions.
maliceSemantics :: StatementListPos -> Either SemError VarTypes
maliceSemantics sl
  = evalState (runErrorT $ semantics sl) (M.empty, newPos "(unknown)" 0 0)