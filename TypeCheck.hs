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
    "\"" ++ fn ++ "\"" ++ " (line " ++ show (fst pos) ++
    ", column " ++ show (snd pos) ++ "):\n" ++ mess

type TypeMonad = ErrorT TypeError (State TypeState)

data TypeState = TypeState { fileName :: FileName
                           , position :: Position
                           , symbolTables :: [SymbolTable]
                           , declarationMap :: DeclarationMap
                           } deriving (Eq,Show)

throwTypeError s = do
  state <- get
  throwError (TypeError (fileName state) (position state) s)

-- "Low level" methods on the state
getSem :: (TypeState -> a) -> TypeMonad a
getSem f = fmap f get

getST = fmap head (getSem symbolTables)
putST st = do
  (TypeState fn pos (_ : sts) dm) <-  get
  put (TypeState fn pos (st : sts) dm)
popST = do
  (TypeState fn pos (st : sts) dm) <- get
  put (TypeState fn pos sts dm)
  return st
pushST st = do
  (TypeState fn pos sts dm) <- get
  put (TypeState fn pos (st : sts) dm)

getPos = getSem position
putPos pos = do
  (TypeState fn _ st dm) <- get
  put (TypeState fn pos st dm)

getDM = getSem declarationMap
putDM dm = do
  (TypeState fn pos sts _) <- get
  put (TypeState fn pos sts dm)

-- Methods to get and put stuff
lookupSymbol v = do
  sts <- getSem symbolTables
  return (look sts)
  where
    look [] = Nothing 
    look (st : sts) = case M.lookup v st of
      Nothing -> look sts
      Just t  -> Just t

getIdentifier (SingleElement v) = do 
  declared <- lookupSymbol v
  case declared of
    Nothing -> throwTypeError ("The variable \"" ++ v ++ "\" has not been declared.")
    Just t  -> return t
getIdentifier (ArrayElement v _) = do
  arr <- getIdentifier (SingleElement v)
  case arr of
    MaliceArraySize t _ -> return t
    MaliceArray t       -> return t

-- The mighty AST checker.
astTypeCheck :: AST -> TypeMonad AST
astTypeCheck (AST fn dl)= do
  declMap dl
  dl' <- mapM declaration dl
  return (AST fn dl')
  
-- Declarations
declMap :: DeclarationList -> TypeMonad ()
declMap [] = return ()
declMap ((_, d) : dl) = do
  dm <- getDM
  case M.lookup (declName d) dm of
    Nothing -> putDM (M.insert (declName d) d dm) >> declMap dl
    _       -> throwTypeError ("Trying to declare already declared function " ++
                               declName d ++ ".")

declaration :: Declaration -> TypeMonad Declaration
declaration (pos, d) = dAct d >>= return . (,) pos
  
dAct :: DeclarationAct -> TypeMonad DeclarationAct
dAct f@(Function _ name args t sl) = do
  pushST (M.fromList args)
  sl' <- statementList sl
  st <- popST
  return (Function st name args t sl)

-- Statements checker
statementList :: StatementList -> TypeMonad StatementList
statementList [] = return []
statementList ((pos, Return e) : sl) = expr e >> return [(pos, Return e)]
statementList (s : sl) = do
  s' <- statement s
  sl' <- statementList sl
  return (s' : sl')

statement :: Statement -> TypeMonad Statement
statement (pos, sact) = putPos pos >> fmap ((,) pos) (sAct sact)

sAct :: StatementAct -> TypeMonad StatementAct
sAct s@(Assign id e) = do
  t1 <- getIdentifier id
  t2 <- expr e
  if t1 == t2
    then return s
    else throwTypeError ("Trying to assign a value of type " ++ show t2 ++
                         " to \"" ++ show id ++ "\" of type " ++ show t1 ++ ".")
sAct s@(Declare t v) = do
  declared <- lookupSymbol v
  case declared of
    Nothing -> (getST >>= (putST . M.insert v t)) >> return s
    _       -> throwTypeError ("Trying to redeclare variable " ++ v ++ ".")
sAct s@(Decrease id) = getIdentifier id >> return s
sAct s@(Increase id) = getIdentifier id >> return s
sAct s@(Print _) = return s
sAct s@(Get _) = return s
sAct s@(Comment _) = return s
sAct s@(FunctionCallS e) = expr e >> return s
sAct (Until _ e sl) = do
  (st, sl') <- conditional e sl
  return (Until st e sl')
sAct (IfElse blocks) = liftM IfElse (mapM block blocks)
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
    else throwTypeError ("Conditional expressions must be of type " ++
                         show MaliceInt ++ ".")

-- Expression checker
expr :: Expr -> TypeMonad MaliceType
expr (Int _) = return MaliceInt
expr (Char _) = return MaliceChar
expr (String _) = return MaliceString
expr (Id id) = getIdentifier id
expr (UnOp op e) = do
  t <- expr e
  opTypes t op
expr (BinOp op e1 e2) = do
  t1 <- expr e1
  t2 <- expr e2
  if t1 == t2
    then opTypes t1 op
    else throwTypeError ("Trying to apply operator \"" ++ op ++ "\" with arguments" ++
                         " of different types " ++ show t1 ++ " and " ++ show t2 ++
                         ".")
expr (FunctionCall f args) = do
  dm <- getDM
  case M.lookup f dm of
    Just fun -> checkFun fun
    Nothing  -> throwTypeError ("Trying to call undeclared function \"" ++ f ++ "\".")
  where
    checkFun (Function _ _ argsF t _) =
      if length args == length argsF 
        then do {
          argsT <- mapM expr args;
          if and (zipWith (\(_, t1) t2 -> t1 == t2) argsF argsT) then
            return t
          else
            throwTypeError ("Invalid types for the arguments of function \"" ++ f ++ "\".");
          }
        else throwTypeError ("Invalid number of arguments for function \"" ++ f ++
                             "\", " ++ show (length args) ++ " instead of " ++
                             show (length argsF) ++ ".")
          
  
-- Operators and types
opTypes MaliceInt _ = return MaliceInt
opTypes t op = throwTypeError ("The operator \"" ++ op ++
                               "\" can not be used with type" ++ show t ++ ".")

maliceTypeCheck :: AST -> Either TypeError AST
maliceTypeCheck ast@(AST fn _) =
  evalState (runErrorT $ astTypeCheck ast) (TypeState fn (0,0) [] M.empty)