module CodeGen where

import List
import Parser
import CodeCleanup



-- Always from right to left, Intel-style

type Register = Int

data LLcmd
--Copy Dest Src
     = LLCpRegVar Register String
	 | LLCpVarReg String Register
	 | LLCpRegImm Register Int
--Set Dest value
	 | LLAdd Register Register
	 | LLMul Register Register
	 | LLDec Register
	 | LLInc Register
--Return: Have the return value ready in register 0!
	 | LLRet
     deriving (Show, Eq)



llProgram :: Program -> [LLcmd]
llProgram (Program statlist)
  = (llStatlist statlist (0, 4))

llStatlist :: StatementList -> (Register, Register) -> [LLcmd]
llStatlist ((Declare var) : ss) (destreg, maxreg)
  = llStatlist ss (destreg, maxreg)
llStatlist ((Assign var exp) : ss) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [(LLCpVarReg var destreg)]
	++ (llStatlist ss (destreg, maxreg))
llStatlist ((Decrease var) : ss) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llStatlist ((Increase var) : ss) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llStatlist ((Return exp) : ss) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [LLRet]
llStatlist [] _
  = []


llExp :: Exp -> (Register, Register) -> [LLcmd]
llExp (BinOp op exp1 exp2) (destreg, maxreg)
  = (llExp exp1 (destreg, maxreg))
	++ (llExp exp2 ((destreg + 1), maxreg))
	++ [llBinOp op destreg (destreg + 1)]
--llExp (UnOp "~" exp)
--  = [Set "ans" 42]
llExp (Int i) (destreg, maxreg)
  = [LLCpRegImm destreg i]
llExp (Var var) (destreg, maxreg)
  = [LLCpRegVar destreg var]


llBinOp :: String -> Register -> Register -> LLcmd
llBinOp "+" i j
  = LLAdd i j
llBinOp "*" i j
  = LLMul i j




optimProgram :: Program -> Program
optimProgram (Program statlist)
  = Program (optimStatement statlist)

optimStatement :: StatementList -> StatementList
optimStatement ( s : ss )
  = optimStatement ss
optimStatement []
  = []

