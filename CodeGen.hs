module CodeGen where

import List
import Parser
import CodeCleanup



-- Always from right to left, Intel-style

type Operand = String
type Register = Int
type Variable = String

data LLcmd
--Copy Dest Src
     = LLCpRegVar Register Variable
	 | LLCpVarReg Variable Register
	 | LLCpRegImm Register Int
	 | LLCpVarImm Variable Int
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
	 | LLAddImm Register Int
	 | LLSubImm Register Int
	 | LLMulImm Register Int
	 | LLDivImm Register Int
	 | LLModImm Register Int
	 | LLAndImm Register Int
	 | LLOrImm Register Int
	 | LLXOrImm Register Int
--Return: Have the return value ready in register 0!
	 | LLRet
     deriving (Show, Eq)




llProgram :: Program -> [LLcmd]
llProgram (Program statlist)
  = (llStatlist statlist (0, 4))



llStatlist :: StatementList -> (Register, Register) -> [LLcmd]
llStatlist (s : ss) (destreg, maxreg)
  = (llStat s (destreg, maxreg))
	++ (llStatlist ss (destreg, maxreg))
llStatlist [] _
  = []



llStat :: Statement -> (Register, Register) -> [LLcmd]
llStat (Declare var) (destreg, maxreg)
  = []
llStat (Assign var (Int imm)) (destreg, maxreg)
  = [LLCpVarImm var imm]
llStat (Assign var exp) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [(LLCpVarReg var destreg)]
llStat (Decrease var) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llStat (Increase var) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llStat (Return exp) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [LLRet]



llExp :: Exp -> (Register, Register) -> [LLcmd]
llExp (BinOp op (Int imm1) (Int imm2)) (destreg, maxreg)
  = [LLCpRegImm destreg (evalBinOp op imm1 imm2)]
llExp (BinOp op (Int imm) exp2) (destreg, maxreg)
  = (llExp exp2 (destreg, maxreg))
	++ [llBinOpImm op destreg imm]
llExp (BinOp op exp1 (Int imm)) (destreg, maxreg)
  = (llExp exp1 (destreg, maxreg))
	++ [llBinOpImm op destreg imm]
llExp (BinOp op exp1 exp2) (destreg, maxreg)
  = (llExp exp1 (destreg, maxreg))
	++ (llExp exp2 ((succ destreg), maxreg))
	++ [llBinOp op destreg (succ destreg)]
llExp (UnOp op (Int imm)) (destreg, maxreg)
  = [LLCpRegImm destreg (evalUnOp op imm)]
llExp (UnOp op exp) (destreg, maxreg)
  = (llExp exp (destreg, maxreg))
	++ [llUnOp op destreg]
--llExp (Int i) (destreg, maxreg)
--  = [LLCpRegImm destreg i]
llExp (Var var) (destreg, maxreg)
  = [LLCpRegVar destreg var]



llBinOp :: Operand -> Register -> Register -> LLcmd
llBinOp "+" rd rs
  = LLAdd rd rs
llBinOp "-" rd rs
  = LLSub rd rs
llBinOp "*" rd rs
  = LLMul rd rs
llBinOp "/" rd rs
  = LLDiv rd rs
llBinOp "%" rd rs
  = LLMod rd rs
llBinOp "&" rd rs
  = LLAnd rd rs
llBinOp "|" rd rs
  = LLAnd rd rs
llBinOp "^" rd rs
  = LLAnd rd rs

llUnOp :: Operand -> Register -> LLcmd
llUnOp "~" rd
  = LLNot rd

llBinOpImm :: Operand -> Register -> Int -> LLcmd
llBinOpImm "+" rd imm
  = LLAddImm rd imm
llBinOpImm "-" rd imm
  = LLSubImm rd imm
llBinOpImm "*" rd imm
  = LLMulImm rd imm
llBinOpImm "/" rd imm
  = LLDivImm rd imm
llBinOpImm "%" rd imm
  = LLModImm rd imm
llBinOpImm "&" rd imm
  = LLAndImm rd imm
llBinOpImm "|" rd imm
  = LLAndImm rd imm
llBinOpImm "^" rd imm
  = LLAndImm rd imm

evalBinOp :: Operand -> Int -> Int -> Int
evalBinOp "+" i j
  = i + j
evalBinOp "-" i j
  = i - j
evalBinOp "*" i j
  = i * j
evalBinOp "/" i j
  = i `div` j
evalBinOp "%" i j
  = i `mod` j
--evalBinOp "&" i j
--  = i & j
--evalBinOp "|" i j
--  = i | j
--evalBinOp "^" i j
--  = i ^ j

evalUnOp :: Operand -> Int -> Int
evalUnOp "~" rd
  = 255 - 0

optimProgram :: Program -> Program
optimProgram (Program statlist)
  = Program (optimStatement statlist)

optimStatement :: StatementList -> StatementList
optimStatement ( s : ss )
  = optimStatement ss
optimStatement []
  = []

