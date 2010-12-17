module OptimiseExpr
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
exprWeight :: Expr -> Int
exprWeight (BinOp _ exp1 exp2)
  = max (exprWeight exp1) ((exprWeight exp2) + 1)
exprWeight (UnOp _ exp1)
  = exprWeight exp1
exprWeight (Int _)
  = 0
exprWeight (Char _)
  = 0
exprWeight (Id _)
  = 0
exprWeight (String _)
  = 0
exprWeight (FunctionCall _ _)
  = 1337



flipBinOpArgs :: Expr -> Expr
flipBinOpArgs (BinOp op e e')
  | op `elem` ["+", "*", "^", "&", "|", "==", "&&", "||", "!="]
     = ( BinOp op e' e )
  | otherwise = ( BinOp op e e' )
flipBinOpArgs e = e

boolToExpr :: Bool -> Expr
boolToExpr True = Int 1
boolToExpr False = Int 0

evalUnOp :: Operand -> Immediate -> Immediate
evalUnOp "~" = (255 -)
evalUnOp "-" = (0 -)
evalUnOp op = error ("evalUnOp: Invalid operand encountered: " ++ op)

-- Do not evaluate division to avoid integer rounding errors
optimiseExpr :: Expr -> Expr
--
-- Immediates
optimiseExpr e@(BinOp "/" (Int _) (Int _)) = e
optimiseExpr e@(BinOp "%" (Int _) (Int _)) = e
optimiseExpr (BinOp "+" (Int x) (Int x')) = Int $ x + x'
optimiseExpr (BinOp "-" (Int x) (Int x')) = Int $ x - x'
optimiseExpr (BinOp "*" (Int x) (Int x')) = Int $ x * x'
optimiseExpr (BinOp "^" (Int x) (Int x')) = Int $ x `xor` x'
optimiseExpr (BinOp "&" (Int x) (Int x')) = Int $ x .&. x'
optimiseExpr (BinOp "|" (Int x) (Int x')) = Int $ x .|. x'
optimiseExpr (BinOp "==" (Int x) (Int x')) = boolToExpr $ x == x'
optimiseExpr (BinOp "<" (Int x) (Int x')) = boolToExpr $ x < x'
optimiseExpr (BinOp ">" (Int x) (Int x')) = boolToExpr $ x > x'
optimiseExpr (BinOp ">=" (Int x) (Int x')) = boolToExpr $ x >= x'
optimiseExpr (BinOp "<=" (Int x) (Int x')) = boolToExpr $ x <= x'
optimiseExpr (BinOp "&&" (Int 1) e) = e
optimiseExpr (BinOp "&&" (Int 0) _) = Int 0
optimiseExpr (BinOp "||" (Int 1) _) = Int 1
optimiseExpr (BinOp "||" (Int 0) e) = e
optimiseExpr (BinOp "!=" (Int x) (Int x')) = boolToExpr $ x == x'
optimiseExpr (BinOp op e e') = BinOp op (optimiseExpr e) (optimiseExpr e')
optimiseExpr (UnOp "-" (Int x)) = Int $ 0 - x
optimiseExpr (UnOp "~" (Int x)) = Int $ complement x
optimiseExpr (UnOp op e) = UnOp op $ optimiseExpr e
--
-- Expression types
-- Evaluate complex expressions first, then variables and finally immediates
optimiseExpr (BinOp op (Int i) (Id v))
  = flipBinOpArgs $ BinOp op (Int i) (optimiseExpr (Id v))
optimiseExpr (BinOp op (Int i) exp2)
  = flipBinOpArgs $ BinOp op (Int i) (optimiseExpr exp2)
optimiseExpr (BinOp op (Id v) exp2)
  = flipBinOpArgs $ BinOp op (optimiseExpr (Id v)) (optimiseExpr exp2)
--
-- Expression weighting
-- Evaluate more costly (in terms of registers) expressions first
optimiseExpr (BinOp op exp1 exp2)
  | exprWeight exp1 > exprWeight exp2
    = flipBinOpArgs $ BinOp op (optimiseExpr exp2) (optimiseExpr exp1)
  | otherwise = BinOp op (optimiseExpr exp1) (optimiseExpr exp2)
optimiseExpr e = e 