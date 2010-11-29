module TypeCheck
       (
         maliceTypeCheck,
       )         
       where

import Common
import Parser
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State

type Message = String
data TypeError = TypeError FileName Position Message

instance Error TypeError where
  noMsg  = TypeError "" (0, 0) "Unknown error."
  strMsg = TypeError "" (0, 0)

instance Show TypeError where
  show (TypeError fn pos mess) =
    show fn ++ show pos ++ "\n" ++ mess ++ "\n"

type TypeMonad = ErrorT TypeError (State TypeState)

data TypeState = TypeState { fileName :: FileName
                           , position :: Position
                           , returnType :: MaliceType
                           , symbolTables :: [SymbolTable]
                           } deriving (Eq,Show)

throwTypeError s = do
  state <- get
  throwError (TypeError (fileName state) (position state) s)

-- "Low level" methods on the state
getSem :: (TypeState -> a) -> TypeMonad a
getSem f = get >>= return . f

getST = getSem symbolTables >>= return . head
putST st = do
  (TypeState fn pos t (_ : sts)) <-  get
  put (TypeState fn pos t (st : sts))
popST = do
  (TypeState fn pos t (st : sts)) <- get
  put (TypeState fn pos t sts)
  return st
pushST st = do
  (TypeState fn pos t sts) <- get
  put (TypeState fn pos t (st : sts))

getPos = getSem position
putPos pos = do
  (TypeState fn _ t st) <- get
  put (TypeState fn pos t st)

getType = getSem returnType
putType t = do
  (TypeState fn pos _ st) <- get
  put (TypeState fn pos t st)
  return st

-- Methods to get and put stuff
lookupSymbol v = do
  sts <- getSem symbolTables
  return (look sts)
  where
    look [] = Nothing 
    look (st : sts) = case M.lookup v st of
      Nothing -> look sts
      Just t  -> Just t

getSymbol v = do
  declared <- lookupSymbol v
  case declared of
    Nothing -> throwTypeError ("The variable " ++ v ++ " has not been declared.")
    Just t  -> return t

getIdentifier (Single v) = getSymbol v
getIdentifier (Array v _) = do
  arr <- getSymbol v
  case arr of
    MaliceArraySize t _ -> return t
    MaliceArray t       -> return t

-- The mighty AST checker.
astTypeCheck :: AST -> TypeMonad AST
astTypeCheck (AST fn _ sl ds) = do
  sl' <- statementList sl
  [st] <- getSem symbolTables
  return (AST fn st sl' ds)
  
-- Statements checker
statementList :: StatementList -> TypeMonad StatementList
statementList = mapM statement

statement :: Statement -> TypeMonad Statement
statement (pos, sact) = putPos pos >> (sAct sact >>= return . ((,) pos))

sAct :: StatementAct -> TypeMonad StatementAct
sAct s@(Assign id e) = getIdentifier id >> expr e >> return s
sAct s@(Declare t v) = do
  declared <- lookupSymbol v
  case declared of
    Nothing -> (getST >>= (\st -> putST (M.insert v t st))) >> return s
    _       -> throwTypeError ("Trying to redeclare variable " ++ v ++ ".")
sAct s@(Decrease id) = getIdentifier id >> return s
sAct s@(Increase id) = getIdentifier id >> return s
sAct s@(Return e) = do
  retT <- getType
  eT <- expr e
  if retT == eT
    then return s
    else throwTypeError ("Trying to return " ++ show eT ++ ", should be " ++
                         show retT ++ ".")
sAct s@(Print _) = return s
sAct s@(Get _) = return s
sAct s@(ProgramDoc _) = return s
sAct (Until _ e sl) = do
  (st, sl') <- conditional e sl
  return (Until st e sl')
sAct (IfElse blocks) = do
  liftM (IfElse) (mapM block blocks)
  where
    block (_, e, sl) = do
      (st, sl') <- conditional e sl
      return (st, e, sl')

conditional e sl = do
  t <- expr e
  if t == MaliceInt
    then do {pushST M.empty;
              sl' <- statementList sl;
              st <- popST;
              return (st, sl');}
    else throwTypeError ("Conditional expressions must be of type" ++ (show MaliceInt))

-- Expression checker
expr :: Expr -> TypeMonad MaliceType
expr (Int _) = return MaliceInt
expr (Char _) = return MaliceChar
expr (String _) = return MaliceString
expr (Id id) = getIdentifier id >>= return
expr (UnOp op e) = do
  t <- expr e
  opTypes t op
expr (BinOp op e1 e2) = do
  t1 <- expr e1
  t2 <- expr e2
  if t1 == t2
    then opTypes t1 op
    else throwTypeError ("Trying to apply operator " ++ op ++ " with arguments" ++
                         " of different types " ++ show t1 ++ " and " ++ show t2 ++
                         ".")
--expr (FunctionOp f args) = -- TODO
  
-- Operators and types
opTypes MaliceInt _ = return MaliceInt
opTypes t op = throwTypeError ("The operator " ++ op ++ " can not be used with " ++
                               show t ++ ".")

runSL :: StatementList -> Either TypeError StatementList
runSL sl
  = evalState (runErrorT $ statementList sl) (TypeState "" (0,0) MaliceInt [M.empty])

maliceTypeCheck :: AST -> Either TypeError AST
maliceTypeCheck ast = evalState (runErrorT $ astTypeCheck ast) (TypeState "" (0,0) MaliceInt [M.empty])