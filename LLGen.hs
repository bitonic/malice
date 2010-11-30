module LLGen
       (
         Operand, Register, Variable, Immediate, LLcmd (..), LLParam (..),
         maliceLL,
       ) where

import Common
import OptimExpr
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

data LLParam = PVar Variable
             | PReg Register
             | PImm Immediate
             deriving (Show, Eq)


data LLcmd
--Copy Dest Src
     = LLCp LLParam LLParam
--Set Dest value
     | LLAdd LLParam LLParam
     | LLSub LLParam LLParam
     | LLMul LLParam LLParam
     | LLDiv LLParam LLParam
     | LLMod LLParam LLParam
     | LLAnd LLParam LLParam
     | LLOr LLParam LLParam
     | LLXOr LLParam LLParam
     | LLDec LLParam
     | LLInc LLParam
     | LLNot LLParam
--Return: Have the return value ready in register 0!
     | LLRet
     | LLSpSub Immediate
     | LLSpAdd Immediate
--     | LLPush Register
--     | LLPushImm Immediate
--     | LLPop Register
     | LLPush LLParam
     | LLPop LLParam
     | LLSrcLine Immediate
     deriving (Show, Eq)






llBinOp :: Operand -> LLParam -> LLParam -> LLcmd
llBinOp "+" = LLAdd
llBinOp "-" = LLSub
llBinOp "*" = LLMul
llBinOp "/" = LLDiv
llBinOp "%" = LLMod
llBinOp "&" = LLAnd
llBinOp "|" = LLOr
llBinOp "^" = LLXOr
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> LLParam -> LLcmd
llUnOp "~" = LLNot
llUnOp op = error ("llUnOp: Invalid operand encountered: " ++ op)

truncates32tou8 :: Immediate -> Immediate
truncates32tou8 i = 255 .&. i
{-
  = if (i < 0)
    then (256 - (i .&. 255))
    else (i .&. 255)
-}




-- The following code does a simple stack machine for evaluating Exprs.
llExp :: Expr -> Register -> [LLcmd]
llExp (BinOp op exp1 (Int imm)) destreg
  = (llExp exp1 destreg)
	++ [llBinOp op (PReg destreg) (PImm imm)]
llExp (BinOp op exp1 exp2) destreg
  = (llExp exp1 destreg)
	++ [LLPush (PReg destreg)]
--	++ (llExp exp2 (succ destreg))
	++ (llExp exp2 destreg)
	++ [LLCp (PReg (succ destreg)) (PReg destreg)]
	++ [LLPop (PReg destreg)]
	++ [llBinOp op (PReg destreg) (PReg (succ destreg))]
llExp (UnOp op (Int imm)) destreg
  = [LLCp (PReg destreg) (PImm (evalUnOp op imm))]
llExp (UnOp op exp1) destreg
  = (llExp exp1 destreg)
	++ [llUnOp op (PReg destreg)]
llExp (Int i) destreg
  = [LLCp (PReg destreg) (PImm i)]
llExp (Char c) destreg
  = [LLCp (PReg destreg) (PImm (truncates32tou8 $ fromIntegral (ord c) :: Immediate))]
llExp (Id (SingleElement var)) destreg
  = [LLCp (PReg destreg) (PVar var)]


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


llS :: StatementAct -> Register -> [LLcmd]
llS (Declare _ _) _
  = []
llS (Assign (SingleElement var) (Int imm)) _
  = [LLCp (PVar var) (PImm imm)]
llS (Assign (SingleElement var) exp1) destreg
  = (llExp (optimiseExpr exp1) destreg) ++ [(LLCp (PVar var) (PReg destreg))]
llS (Decrease (SingleElement var)) destreg
  = [(LLCp (PReg destreg) (PVar var)), (LLDec (PReg destreg)), (LLCp (PVar var) (PReg destreg))]
llS (Increase (SingleElement var)) destreg
  = [(LLCp (PReg destreg) (PVar var)), (LLInc (PReg destreg)), (LLCp (PVar var) (PReg destreg))]
llS (Return exp1) destreg
  = (llExp (optimiseExpr exp1) destreg) ++ [LLRet]


llSA :: Statement -> Register -> [LLcmd]
llSA s destreg
  = (LLSrcLine $ fromIntegral line) : (llS sa destreg)
  where
    ((line, _), sa) = s

llSL :: StatementList -> Register -> [LLcmd]
llSL ss destreg
  = concat $ map (flip llSA destreg) ss


maliceLL :: StatementList -> [LLcmd]
maliceLL sl = llSL sl 0
