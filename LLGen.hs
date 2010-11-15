module LLGen
       (
         Operand, Register, Variable, Immediate, LLcmd (..),
         maliceLL,
       ) where

import Parser
import Data.Int (Int32)
import Data.Bits
import Data.Char


type Operand = String
type Register = Int
type Variable = String
type Immediate = Int32

--type VarMap = [(Variable, String)]

--type VarOffset = Int
--data VarValue = Imm Immediate
--data VarInfo = SVar VarOffset VarValue

--data LLParam = PVar VarInfo | PReg Register | PImm Immediate


data LLcmd
--Copy Dest Src
     = LLCpRegVar Register Variable
     | LLCpVarReg Variable Register
     | LLCpRegImm Register Immediate
     | LLCpVarImm Variable Immediate
     | LLCpRegReg Register Register
--Set Dest value
     | LLAdd Register Register
     | LLSub Register Register
     | LLMul Register Register
     | LLDiv Register Register
     | LLMod Register Register
     | LLAnd Register Register
     | LLOr Register Register
     | LLXOr Register Register
     | LLDec Register
     | LLInc Register
     | LLNot Register
     | LLAddImm Register Immediate
     | LLSubImm Register Immediate
     | LLMulImm Register Immediate
     | LLDivImm Register Immediate
     | LLModImm Register Immediate
     | LLAndImm Register Immediate
     | LLOrImm Register Immediate
     | LLXOrImm Register Immediate
--Return: Have the return value ready in register 0!
     | LLRet
     | LLSpSub Immediate
     | LLSpAdd Immediate
     | LLPush Register
--     | LLPushImm Immediate
     | LLPop Register
     deriving (Show, Eq)


-- Optimisations disabled for now
{-

exprIntermeds :: Expr -> Int
exprIntermeds (BinOp _ exp1 exp2)
  = max (exprIntermeds exp1) ((exprIntermeds exp2) + 1)
exprIntermeds (UnOp _ exp1)
  = exprIntermeds exp1
exprIntermeds (Int _)
  = 0
exprIntermeds (Char _)
  = 0
exprIntermeds (Var _)
  = 0

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
sortExprType (BinOp op (Int i) (Var v))
  = flipBinOpArgs $ BinOp op (sortExprType (Var v)) (sortExprType (Int i))
sortExprType (BinOp op (Int i) exp2)
  = flipBinOpArgs $ BinOp op (sortExprType (Int i)) (sortExprType exp2)
sortExprType (BinOp op (Var v) exp2)
  = flipBinOpArgs $ BinOp op (sortExprType (Var v)) (sortExprType exp2)
sortExprType exp1
  = exp1

reduceExprImms' :: Expr -> Expr
reduceExprImms' e@(BinOp "/" (Int _) (Int 0))
  = e
reduceExprImms' e@(BinOp "%" (Int _) (Int 0))
  = e
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
  | op1 == op2
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
evalBinOp "/" = div
evalBinOp "%" = mod
evalBinOp "&" = (.&.)
evalBinOp "|" = (.|.)
evalBinOp "^" = xor
evalBinOp op = error ("evalBinOp: Invalid operand encountered: " ++ op)


-}

evalUnOp :: Operand -> Immediate -> Immediate
evalUnOp "~" = (255 -)
evalUnOp op = error ("evalUnOp: Invalid operand encountered: " ++ op)



optimiseExpr :: Expr -> Expr
--optimiseExpr = reduceExprImms . sortExprType . sortExprWeight
--optimiseExpr = sortExprType . sortExprWeight
-- Disable optimisations for now
optimiseExpr = id




llSL :: StatementList -> Register -> [LLcmd]
llSL (s : ss) destreg
  = (llS s destreg)
    ++ (llSL ss destreg)
llSL [] _
  = []

llS :: Statement -> Register -> [LLcmd]
llS (Declare _ _) _
  = []
llS (Assign var (Int imm)) _
  = [LLCpVarImm var imm]
llS (Assign var exp1) destreg
  = (llExp (optimiseExpr exp1) destreg) ++ [(LLCpVarReg var destreg)]
llS (Decrease var) destreg
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llS (Increase var) destreg
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llS (Return exp1) destreg
  = (llExp (optimiseExpr exp1) destreg) ++ [LLRet]

{-
 - This code is for unlimited registers, to be used later
 -
llExp :: Expr -> Register -> [LLcmd]
llExp (BinOp op exp1 (Int imm)) destreg
  = (llExp exp1 destreg)
	++ [llBinOpImm op destreg imm]
llExp (BinOp op exp1 exp2) destreg
  = (llExp exp1 destreg)
	++ (llExp exp2 (succ destreg))
	++ [llBinOp op destreg (succ destreg)]
llExp (UnOp op (Int imm)) destreg
  = [LLCpRegImm destreg (evalUnOp op imm)]
llExp (UnOp op exp1) destreg
  = (llExp exp1 destreg)
	++ [llUnOp op destreg]
llExp (Int i) destreg
  = [LLCpRegImm destreg i]
llExp (Char c) destreg
  = [LLCpRegImm destreg (truncate32to8 $ fromIntegral (ord c) :: Immediate)]
llExp (Var var) destreg
  = [LLCpRegVar destreg var]
-}

-- The following code does a simple stack machine for evaluating Exprs.
llExp :: Expr -> Register -> [LLcmd]
llExp (BinOp op exp1 (Int imm)) destreg
  = (llExp exp1 destreg)
	++ [llBinOpImm op destreg imm]
llExp (BinOp op exp1 exp2) destreg
  = (llExp exp1 destreg)
	++ [LLPush destreg]
--	++ (llExp exp2 (succ destreg))
	++ (llExp exp2 destreg)
	++ [LLCpRegReg (succ destreg) destreg]
	++ [LLPop destreg]
	++ [llBinOp op destreg (succ destreg)]
llExp (UnOp op (Int imm)) destreg
  = [LLCpRegImm destreg (evalUnOp op imm)]
llExp (UnOp op exp1) destreg
  = (llExp exp1 destreg)
	++ [llUnOp op destreg]
llExp (Int i) destreg
  = [LLCpRegImm destreg i]
llExp (Char c) destreg
  = [LLCpRegImm destreg (truncates32tou8 $ fromIntegral (ord c) :: Immediate)]
llExp (Var var) destreg
  = [LLCpRegVar destreg var]




llBinOp :: Operand -> Register -> Register -> LLcmd
llBinOp "+" = LLAdd
llBinOp "-" = LLSub
llBinOp "*" = LLMul
llBinOp "/" = LLDiv
llBinOp "%" = LLMod
llBinOp "&" = LLAnd
llBinOp "|" = LLOr
llBinOp "^" = LLXOr
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> Register -> LLcmd
llUnOp "~" = LLNot
llUnOp op = error ("llUnOp: Invalid operand encountered: " ++ op)

llBinOpImm :: Operand -> Register -> Immediate -> LLcmd
llBinOpImm "+" = LLAddImm
llBinOpImm "-" = LLSubImm
llBinOpImm "*" = LLMulImm
llBinOpImm "/" = LLDivImm
llBinOpImm "%" = LLModImm
llBinOpImm "&" = LLAndImm
llBinOpImm "|" = LLOrImm
llBinOpImm "^" = LLXOrImm
llBinOpImm op = error ("llBinOpImm: Invalid operand encountered: " ++ op)




truncates32tou8 :: Immediate -> Immediate
truncates32tou8 i = 255 .&. i
{-
  = if (i < 0)
    then (256 - (i .&. 255))
    else (i .&. 255)
-}

maliceLL :: StatementList -> [LLcmd]
maliceLL sl = llSL sl 0
