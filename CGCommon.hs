module CGCommon
       (
         Operand, Register, Variable, Immediate, Label,
         ScopeInfo, SIM,
         getFuncName, putFuncName,
         getSymTabs, putSymTabs, pushSymTab, popSymTab, lookupSym,
         getStrTab, putStrTab, uniqStr,
         getCodePos, putCodePos, showCodePos,
         getLabelCtr, putLabelCtr, uniqLabel,
       ) where

import Common
import Data.Int (Int32)
--import Data.Map ( Map )
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State


type Operand = String
type Register = Int
type Variable = String
type Immediate = Int32
type Label = String


--type StringTable = Map Int String
type StringTable = [(Int, String)]


type ScopeInfo = (String, [SymbolTable], StringTable, (Int, Int), Int)
-- ScopeInfo Monad
type SIM = State ScopeInfo


getFuncName :: SIM String
getFuncName = do
  (fn, _, _, _, _) <- get
  return fn

putFuncName :: String -> SIM ()
putFuncName fn = do
  (_, syt, stt, cp, lc) <- get
  put (fn, syt, stt, cp, lc)

getSymTabs :: SIM [SymbolTable]
getSymTabs = do
  (_, symtab, _, _, _) <- get
  return symtab

putSymTabs :: [SymbolTable] -> SIM ()
putSymTabs syt = do
  (fn, _, stt, cp, lc) <- get
  put (fn, syt, stt, cp, lc)

pushSymTab :: SymbolTable -> SIM ()
pushSymTab syt = do
  sts <- getSymTabs
  putSymTabs ((prepSymTabOffsets (sum $ map M.size sts) syt) : sts)

popSymTab :: SIM SymbolTable
popSymTab = do
  (syt : sts) <- getSymTabs
  putSymTabs sts
  return syt

lookupSym :: String -> SIM (Maybe (MaliceType, Int))
lookupSym v = do
  sts <- getSymTabs
  return $ case filter isJust $ map (M.lookup v) sts of
    [ ] -> Nothing
    (x : _) -> x

prepSymTabOffsets' :: Int -> [(Variable, (MaliceType, Int))] ->  [(Variable, (MaliceType, Int))]
prepSymTabOffsets' _ []
  = []
prepSymTabOffsets' num ( (v, (t, _)) : ss )
  = (v, (t, num)) : prepSymTabOffsets' (succ num) ss

prepSymTabOffsets :: Int -> SymbolTable -> SymbolTable
prepSymTabOffsets startid = M.fromList . (prepSymTabOffsets' startid) . M.toAscList



getStrTab :: SIM StringTable
getStrTab = do
  (_, _, strtab, _, _) <- get
  return strtab

putStrTab :: StringTable -> SIM ()
putStrTab stt = do
  (fn, syt, _, cp, lc) <- get
  put (fn, syt, stt, cp, lc)

uniqStr :: String -> SIM Label
uniqStr str = do
  stt <- getStrTab
  sid <- return $ length stt
--  fn <- getFunName
  putStrTab $ (sid, str) : stt
  return $ "_str_" ++ (show sid)


getCodePos :: SIM (Int, Int)
getCodePos = do
  (_, _, _, cp, _) <- get
  return cp

putCodePos :: (Int, Int) -> SIM ()
putCodePos cp = do
  (fn, syt, stt, _, lc) <- get
  put (fn, syt, stt, cp, lc)

showCodePos :: SIM String
showCodePos = do
  (line, col) <- getCodePos
  return $ "In line " ++ (show line) ++ ":" ++ (show col) ++ ": "


getLabelCtr :: SIM Int
getLabelCtr = do
  (_, _, _, _, lc) <- get
  return lc

putLabelCtr :: Int -> SIM ()
putLabelCtr lc = do
  (fn, syt, stt, cp, _) <- get
  put (fn, syt, stt, cp, lc)

uniqLabel :: SIM String
uniqLabel = do
  fn <- getFuncName
  lc <- getLabelCtr
  putLabelCtr (lc + 1)
  return $ "_" ++ fn ++ "_" ++ (show lc)

