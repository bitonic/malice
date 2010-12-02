module LLGen
       (
         LLcmd (..), LLParam (..),
         maliceLL,
       ) where

import Common
import CGCommon
import OptimExpr
import Data.Bits
import Data.Char


--type VarOffset = Int
--data VarValue = Imm Immediate
--data VarInfo = SVar VarOffset VarValue

data LLParam = PVar Variable
             | PReg Register
             | PImm Immediate
             | PStr String
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
     | LLPrint LLParam
     | LLGet Variable
     | LLCall String
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
llExp (FunctionCall fn args) destreg
  = (if destreg == 0 then [] else [LLPush (PReg 0)])
    ++ ( concat $ map (flip (++) [LLPush (PReg destreg)]) $ map (flip (llExp) destreg) args )
    ++ [LLCall fn, LLSpAdd $ fromIntegral (4 * length args)]
    ++ (if destreg == 0 then [] else [LLCp (PReg destreg) (PReg 0), LLPop (PReg 0)])
llExp (String _) _
  = error "Implement EXP String"
llExp (Id (ArrayElement _ _)) _
  = error "Implement EXP ArrayElement"


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


llSA :: StatementAct -> [LLcmd]
llSA (Declare _ _)
  = []
llSA (Assign (SingleElement var) (Int imm))
  = [LLCp (PVar var) (PImm imm)]
llSA (Assign (SingleElement var) exp1)
  = (llExp (optimiseExpr exp1) 0) ++ [(LLCp (PVar var) (PReg 0))]
llSA (Assign (ArrayElement _ _) _)
  = error "Implement Assign ArrayElement"
llSA (Decrease (SingleElement var))
  = [(LLCp (PReg 0) (PVar var)), (LLDec (PReg 0)), (LLCp (PVar var) (PReg 0))]
llSA (Decrease (ArrayElement _ _))
  = error "Implement Decrease ArrayElement"
llSA (Increase (SingleElement var))
  = [(LLCp (PReg 0) (PVar var)), (LLInc (PReg 0)), (LLCp (PVar var) (PReg 0))]
llSA (Increase (ArrayElement _ _))
  = error "Implement Increase ArrayElement"
llSA (Return exp1)
  = (llExp (optimiseExpr exp1) 0) ++ [LLRet]
llSA (Print (String str))
  = [LLPrint (PStr str)]
llSA (Print exp1)
  = (llExp (optimiseExpr exp1) 0) ++ error "Implement LLPrint"
llSA (Get (SingleElement var))
  = (llExp (FunctionCall "_readint" []) 0) ++ [LLCp (PVar var) (PReg 0)]
llSA (Get (ArrayElement _ _))
  = error "Cannot read a whole array"
llSA (Comment _)
  = []
llSA (FunctionCallS fc@(FunctionCall _ _))
  = llExp fc 0
llSA (FunctionCallS _)
  = error "Cannot call an expression that is not a FunctionCall"
llSA (Until _ _ _)
  = error "Implement LLUntil"
llSA (IfElse _)
  = error "Implement LLIfElse"


llS :: Statement -> [LLcmd]
llS s
  = (LLSrcLine $ fromIntegral line) : (llSA sa)
  where
    ((line, _), sa) = s

llSL :: StatementList -> [LLcmd]
llSL ss
  = concat $ map (llS) ss


maliceLL :: StatementList -> [LLcmd]
maliceLL sl = llSL sl
