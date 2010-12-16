module CGCommon
       (
         Operand, Register, Variable, Immediate, FunctionName, Label, StringTable, MaxVarCount,
         ScopeInfo, SIM,
         getFuncName, putFuncName,
         getSymTabs, putSymTabs, pushSymTab, scanSymTab, popSymTab, lookupSym, funcArgsSymTab,
         getStrTab, putStrTab, uniqStr,
         getCodePos, putCodePos, showCodePos,
         getLabelCtr, putLabelCtr, uniqLabel,
         getMaxVarCtr, putMaxVarCtr,
         strToAsm,
       ) where

import Common
import Data.Int (Int32)
import Char ( ord )
--import Data.Map ( Map )
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State


type Operand = String
type Register = Int
type Variable = String
type Immediate = Int32

type FunctionName = Label
type Label = String
type LabelCounter = Int
type MaxVarCount = Int


--type StringTable = Map Int String
type StringTable = [(Int, String)]


type ScopeInfo = (FunctionName,
                  SymbolTable,
                  [SymbolTable],
                  StringTable,
                  Position,
                  LabelCounter,
                  MaxVarCount
                 )
-- ScopeInfo Monad
type SIM = State ScopeInfo
--  --  --  --  --  --  --   Func args  -- Local vars --  --  --  --  Code Pos. --
--type LLInfo = (FunctionName, SymbolTable, [SymbolTable], StringTable, Position)
--type LLMonad = State LLInfo
--type CGMonad = State (FunctionName, StringTable)


getFuncName :: SIM String
getFuncName = do
  (fn, _, _, _, _, _, _) <- get
  return fn

putFuncName :: String -> SIM ()
putFuncName fn = do
  (_, fas, syt, stt, cp, lc, mvc) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)

getFuncArgsTab :: SIM SymbolTable
getFuncArgsTab = do
  (_, fas, _, _, _, _, _) <- get
  return fas

getSymTabs :: SIM [SymbolTable]
getSymTabs = do
  (_, _, symtab, _, _, _, _) <- get
  return symtab

putSymTabs :: [SymbolTable] -> SIM ()
putSymTabs syt = do
  (fn, fas, _, stt, cp, lc, mvc) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)

pushSymTab :: SymbolTable -> SIM ()
pushSymTab syt = do
  sts <- getSymTabs
  putSymTabs (syt : sts)

popSymTab :: SIM SymbolTable
popSymTab = do
  (syt : sts) <- getSymTabs
  putSymTabs sts
  return syt

lookupSym :: String -> SIM (Maybe (MaliceType, Int))
lookupSym v = do
  sts <- getSymTabs
  fas <- getFuncArgsTab
  return $ case filter isJust $ map (M.lookup v) sts of
    [ ] -> case M.lookup v fas of
             Just y -> Just y
             Nothing -> error ("Couldn't find symbol " ++ v)
    (x : _) -> x


symMaxOffset :: [SymbolTable] -> Int
symMaxOffset [] = 0
symMaxOffset (x : xs)
  = case offsets of
      [] -> symMaxOffset xs
      _ -> maximum offsets
  where
    offsets = [ o | (_, (_, o)) <- (M.toAscList x) ]

-- prepares the given symtable based on existing number of entries on the stack
scanSymTab :: SymbolTable -> SIM SymbolTable
scanSymTab syt = do
  sts <- getSymTabs
  oldcount <- return $ symMaxOffset sts
  newsyt <- return $ prepSymTabOffsets (oldcount + 1) syt
  oldmvc <- getMaxVarCtr
  putMaxVarCtr $ max oldmvc (symMaxOffset [newsyt])
  return $ newsyt

typeSize :: MaliceType -> Int
--typeSize (MaliceArray _) = 2
--typeSize (MaliceArraySize _ _) = 2
typeSize _ = 1

--Assign Offsets      Next    The Symbol
prepSymTabOffsets' :: Int -> [(Variable, (MaliceType, Int))] ->  [(Variable, (MaliceType, Int))]
prepSymTabOffsets' _ []
  = []
prepSymTabOffsets' num ( (v, (t, _)) : ss )
  = (v, (t, num - 1 + typeSize t )) : prepSymTabOffsets' (num + typeSize t) ss

prepSymTabOffsets :: Int -> SymbolTable -> SymbolTable
prepSymTabOffsets startid = M.fromList . (prepSymTabOffsets' startid) . M.toAscList

-- A hack to have "valid" offsets for function arguments on the stack
funcArgsSymTab' :: FunctionArgs -> SymbolTable -> SymbolTable
funcArgsSymTab' [] st
  = st
funcArgsSymTab' ((v, t) : xs) st
  = funcArgsSymTab' xs (M.insert v (t, (-3) - (length xs) ) st)

funcArgsSymTab :: FunctionArgs -> SymbolTable
funcArgsSymTab fa
  = funcArgsSymTab' (reverse fa) M.empty


getStrTab :: SIM StringTable
getStrTab = do
  (_, _, _, strtab, _, _, _) <- get
  return strtab

putStrTab :: StringTable -> SIM ()
putStrTab stt = do
  (fn, fas, syt, _, cp, lc, mvc) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)

uniqStr :: String -> SIM Label
uniqStr str = do
  stt <- getStrTab
  sid <- return $ length stt
--  fn <- getFunName
  putStrTab $ (sid, str) : stt
  return $ "_str_" ++ (show sid)


getCodePos :: SIM (Int, Int)
getCodePos = do
  (_, _, _, _, cp, _, _) <- get
  return cp

putCodePos :: (Int, Int) -> SIM ()
putCodePos cp = do
  (fn, fas, syt, stt, _, lc, mvc) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)

showCodePos :: SIM String
showCodePos = do
  (line, col) <- getCodePos
  return $ "In line " ++ (show line) ++ ":" ++ (show col) ++ ": "


getLabelCtr :: SIM Int
getLabelCtr = do
  (_, _, _, _, _, lc, _) <- get
  return lc

putLabelCtr :: Int -> SIM ()
putLabelCtr lc = do
  (fn, fas, syt, stt, cp, _, mvc) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)

uniqLabel :: String -> SIM String
uniqLabel prefix = do
  fn <- getFuncName
  lc <- getLabelCtr
  putLabelCtr (lc + 1)
  return $ "_" ++ prefix ++ "_" ++ fn ++ "_" ++ (show lc)


getMaxVarCtr :: SIM MaxVarCount
getMaxVarCtr = do
  (_, _, _, _, _, _, mvc) <- get
  return mvc

putMaxVarCtr :: MaxVarCount -> SIM ()
putMaxVarCtr mvc = do
  (fn, fas, syt, stt, cp, lc, _) <- get
  put (fn, fas, syt, stt, cp, lc, mvc)




strToAsm s = "\"" ++ strToAsm' s ++ "\",0"
  where
    strToAsm' [] = []
    strToAsm' (c : s')
      | elem c escapedChars = "\"," ++ show (ord c) ++ ",\"" ++ strToAsm' s'
      | otherwise           = c : strToAsm' s'
    escapedChars = "\0\a\b\f\n\r\t\v\"\&\'\\"
