module Reduce
       (
         maliceReduce
       ) where
       
import Parser       
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State

-- Function that reduces malice statements.
maliceReduce :: StatementList -> StatementList
maliceReduce sl = evalState (reduceSL sl) M.empty
  
reduceSL :: StatementList -> State (Map String Expr) StatementList  
reduceSL (Return e : _) = do
  e' <- reduceExpr e
  return [Return e']
reduceSL (Assign v e : sl) = do
  e' <- reduceExpr e
  vars <- get
  put (M.insert v e' vars)
  reduceSL sl
reduceSL (Declare _ _ : sl) = reduceSL sl
reduceSL (Decrease v : sl) = do
  vars <- get
  let (Int i) = vars M.! v in do {
    put (M.insert v (Int (i - 1)) vars);
    reduceSL sl; }
reduceSL (Increase v : sl) = do
  vars <- get
  let (Int i) = vars M.! v in do {
    put (M.insert v (Int (i + 1)) vars);
    reduceSL sl; }

reduceExpr (BinOp op e1 e2) = do
  (Int i1) <- reduceExpr e1
  (Int i2) <- reduceExpr e2
  return (Int (getBinOp op i1 i2))
reduceExpr (UnOp "~" e) = do
  (Int i) <- reduceExpr e
  return (Int $ complement i)
reduceExpr (Int i) = return (Int i)
reduceExpr (Char c) = return (Char c)
reduceExpr (Var v) = do
  vars <- get
  return (vars M.! v)
  
  
getBinOp "+" = (+)
getBinOp "-" = (-)
getBinOp "*" = (*)
getBinOp "/" = div
getBinOp "%" = mod
getBinOp "&" = (.&.)
getBinOp "|" = (.|.)
getBinOp "^" = xor