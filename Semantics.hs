module Semantics
       (
         maliceSemantics,
         SymbolTables
       )
       where

import Common
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

-- The operators types
opTypes MaliceInt = ["+", "-", "*", "/", "%", "^", "&", "|", "~"]
opTypes MaliceChar = []

-- The error type. SourcePos comes from Text.Parsec
type Message = String
data SemError = SemError FileName Position Message

instance Error SemError where
  noMsg  = SemError (0, 0) "Unknown error."
  strMsg = SemError (0, 0)

instance Show SemError where
  show (SemError fn pos mess) =
    show fn ++ pos ++ "\n" ++ mess ++ "\n"

-- The monad, derived from the error monad transformer.
-- The inner monad is a state with the current symbol table and
-- the current position. Probably it's possible to have a cleaner
-- Code using Applicative, but we did not have time to do that
-- for milestone 2.
type SemMonad = ErrorT SemError (State SemState)

data SemState = SemState { fileName :: FileName
                         , position :: Position
                         , returnType :: MaliceType
                         , symbolTable :: SymbolTable
                         } deriving (Eq,Show)

-- Helper function to throw errors
throwSemError s = do
  state <- get
  throwError (SemError (fileName state) (position state) s)

getSem f = get >>= return . f

getST = getSem symbolTable
putST st = do
  (SemState fn pos t _) <- get
  put (SemState fn pos t st)
  
getPos = getSem position
putPos pos = do
  (SemState fn _ t st) <- get
  put (SemState fn pos t st)

getType = getSem returnType
putType t = do
  (SemState fn pos _ st) <- get
  put (SemState fn pos t st)
  return st

insertId (Single s) t st = M.insert s t st
insertId (Array s _) t st = M.insert s t st

lookupId (Single s) st = M.lookup s st
lookupId (Array s _) st = M.lookup s st

getId (Single s) st = st M.! s
getId (Array s _) st = st M.! s
  
-- The actual semantics analysis
semSL :: StatementList -> SemMonad AST
semSL [] = return []
semSL ((_, Return e) : _) = do
  retT <- semExpr
  t <- getType
  if retT == t
    then return []
    else throwSemError ("The return type should be " ++ show t ++
                        " but " ++ show retT ++ " is being returned.")
semSL (s : sl) = do
  s' <- semS s
  semSL sl >>= return . (flip (:) $ s')

semS :: (Position, StatementAct) -> SemMonad Statement
semS s@(pos, Assign id expr) = do
  putPos pos
  checkDecl id (
    \tVar -> do {
      tExpr <- semExpr expr;
      if (tExpr == tVar) then return s
        else throwSemError (
        "Trying to assign a value of type \"" ++ show tExpr ++
        "\" to \"" ++ show id ++ "\" of type \"" ++ show tVar ++ "\".");
      })
semS s@(pos, Declare t id) = do
  st <- putPos pos
  case lookupId id st of
    Nothing -> putST (insertId id t st) >> return s
    _       -> throwSemError ("The \"" ++ show id ++ "\" was already" ++
                              " declared.")
semS s@(pos, Decrease id) = do
  putPos pos
  checkDecl id (
    \t -> case t of
      MaliceInt -> return s
      _         -> throwSemError ("Trying to decrease var \"" ++ v ++
                                  " of type \"" ++ show t ++ "\"."))
semS s@(pos, Increase v) = do
  putPos pos
  checkDecl v (
    \t -> case t of
      MaliceInt -> return s
      _         -> throwSemError ("Trying to increase var \"" ++ v ++
                                  " of type \"" ++ show t ++ "\"."))
semS (_, Return _) = error "The function semS is to be called by semSL."
semS s@(pos, Print e) = putPos pos >> semExpr e >> return s
semS s@(pos, Get id) = putPos pos >> checkDecl id (return s)
semS (pos, ProgramDoc _) = 
  throwError "The program description must be at the beginning of the program."
semS s@(pos, ChangerCall s id) = return s
semS s@(pos, FunctionCall e) = return s
semS (pos, Until _ e sl) = do
  putPos pos
  t <- semExpr e
  case t of
    MaliceInt -> do {
      topST <- getSt;
      putST M.empty;
      st <- semSL sl;
      putST topST
      return (pos, Until st e sl);
      }
    _ -> throwSemError ("Conditional expressions must be of type \"number\".")
semS (pos, Function _ name args t sl) = do  
  putPos pos
  topSt <- getST

checkDecl :: Identifier -> (MaliceType -> SemMonad a) -> SemMonad a
checkDecl id f = do
  st <- getST
  case lookupId id st of
    Nothing  -> throwSemError ("Trying to assign a value to the \"" ++
                               show id ++ "\" which has not been declared.")
    (Just t) -> f t

semExpr :: Expr -> SemMonad MaliceType
semExpr (Int _) = return MaliceInt
semExpr (Char _) = return MaliceChar
semExpr (Var v) = do 
  st <- getST
  return (getId v st)
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
maliceSemantics :: StatementListPos -> Either SemError SymbolTable
maliceSemantics sl
  = evalState (runErrorT $ semantics sl) (M.empty, newPos "(unknown)" 0 0)