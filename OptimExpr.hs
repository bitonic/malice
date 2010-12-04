module OptimExpr
       (
         optimiseExpr,
         evalUnOp,
--         evalBinOp,
       ) where

import Common
import Data.Int (Int32)
import Data.Bits


type Operand = String
type Immediate = Int32


-- Calculate the register cost of a subexpression
exprIntermeds :: Expr -> Int
exprIntermeds (BinOp _ exp1 exp2)
  = max (exprIntermeds exp1) ((exprIntermeds exp2) + 1)
exprIntermeds (UnOp _ exp1)
  = exprIntermeds exp1
exprIntermeds (Int _)
  = 0
exprIntermeds (Char _)
  = 0
exprIntermeds (Id _)
  = 0
exprIntermeds (String _)
  = 0
exprIntermeds (FunctionCall _ _)
  = 1337


flipBinOpArgs :: Expr -> Expr
flipBinOpArgs (BinOp op exp1 exp2)
  | op == "+" || op == "*" || op == "&" || op == "|" || op == "^"
     = ( BinOp op exp2 exp1 )
  | otherwise = ( BinOp op exp1 exp2 )
flipBinOpArgs exp1
  = exp1

-- Evaluate more costly (in terms of registers) expressions first
sortExprWeight :: Expr -> Expr
sortExprWeight (BinOp op exp1 exp2)
  = if exprIntermeds exp1 > exprIntermeds exp2
      then flipBinOpArgs $ BinOp op (sortExprWeight exp2) (sortExprWeight exp1)
      else BinOp op (sortExprWeight exp1) (sortExprWeight exp2)
sortExprWeight exp1
  = exp1

-- Evaluate complex expressions first, then variables and finally immediates
sortExprType :: Expr -> Expr
sortExprType (BinOp op (Int i) (Id v))
  = flipBinOpArgs $ BinOp op (sortExprType (Id v)) (sortExprType (Int i))
sortExprType (BinOp op (Int i) exp2)
  = flipBinOpArgs $ BinOp op (sortExprType (Int i)) (sortExprType exp2)
sortExprType (BinOp op (Id v) exp2)
  = flipBinOpArgs $ BinOp op (sortExprType (Id v)) (sortExprType exp2)
sortExprType exp1
  = exp1


-- Do not evaluate division to avoid integer rounding errors
reduceExprImms' :: Expr -> Expr
reduceExprImms' e@(BinOp "/" (Int _) (Int _))
  = e
reduceExprImms' e@(BinOp "%" (Int _) (Int _))
  = e
--reduceExprImms' e@(BinOp "/" (Int _) (Int 0))
--  = e
--reduceExprImms' e@(BinOp "%" (Int _) (Int 0))
--  = e
reduceExprImms' (BinOp op (Int i1) (Int i2))
  = Int (evalBinOp op i1 i2)
reduceExprImms' (UnOp op (Int i))
  = Int (evalUnOp op i)
reduceExprImms' exp1
  = exp1

-- Collapse immediates from right to left if possible
reduceExprImms :: Expr -> Expr
reduceExprImms (BinOp op2 e1@(BinOp op1 exp1 (Int i1)) e2@(Int i2))
-- FIXME: THIS COLLAPSES ALL OPERATORS, OVERSEEING ROUNDING ERRORS
  | op1 == op2 && op1 /= "/" && op1 /= "%"
    = reduceExprImms' (BinOp op1 (reduceExprImms exp1) (Int (evalBinOp op2 i1 i2)))
  | otherwise
    = reduceExprImms' ( BinOp op2 (reduceExprImms e1) e2 )
reduceExprImms (BinOp op exp1 exp2)
  = reduceExprImms' (BinOp op (reduceExprImms exp1) (reduceExprImms exp2))
reduceExprImms (UnOp op exp1)
  = reduceExprImms' (UnOp op (reduceExprImms exp1))
reduceExprImms exp1
  = reduceExprImms' exp1

evalBinOp :: Operand -> Immediate -> Immediate -> Immediate
evalBinOp "+" = (+)
evalBinOp "-" = (-)
evalBinOp "*" = (*)
evalBinOp "&" = (.&.)
evalBinOp "|" = (.|.)
evalBinOp "^" = xor
evalBinOp op = error ("evalBinOp: Invalid operand encountered: " ++ op)



evalUnOp :: Operand -> Immediate -> Immediate
evalUnOp "~" = (255 -)
evalUnOp op = error ("evalUnOp: Invalid operand encountered: " ++ op)



optimiseExpr :: Expr -> Expr
--optimiseExpr = reduceExprImms . sortExprType . sortExprWeight
--optimiseExpr = sortExprType . sortExprWeight
-- Disable optimisations for now
optimiseExpr = id


