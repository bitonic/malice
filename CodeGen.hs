module CodeGen 
       (
         codeGen,
       ) where

import List
import Parser
import CodeCleanup
import Data.Int (Int32)
import Data.Bits
import Data.Char
import Reduce


-- Always from right to left, Intel-style

type Operand = String
type Register = Int
type Variable = String

data LLcmd
--Copy Dest Src
     = LLCpRegVar Register Variable
	 | LLCpVarReg Variable Register
	 | LLCpRegImm Register Int32
	 | LLCpVarImm Variable Int32
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
	 | LLAddImm Register Int32
	 | LLSubImm Register Int32
	 | LLMulImm Register Int32
	 | LLDivImm Register Int32
	 | LLModImm Register Int32
	 | LLAndImm Register Int32
	 | LLOrImm Register Int32
	 | LLXOrImm Register Int32
--Return: Have the return value ready in register 0!
	 | LLRet
     deriving (Show, Eq)




llProgram :: AST -> [LLcmd]
llProgram (Program statlist)
  = (llStatlist statlist (0, 4))



llStatlist :: StatementList -> (Register, Register) -> [LLcmd]
llStatlist (s : ss) (destreg, maxreg)
  = (llStat s (destreg, maxreg))
	++ (llStatlist ss (destreg, maxreg))
llStatlist [] _
  = []



llStat :: Statement -> (Register, Register) -> [LLcmd]
llStat (Declare _ var) (destreg, maxreg)
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



llExp :: Expr -> (Register, Register) -> [LLcmd]
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
llExp (Int i) (destreg, maxreg)
  = [LLCpRegImm destreg i]
llExp (Char c) (destreg, maxreg)
  = [LLCpRegImm destreg (fromIntegral (ord c) :: Int32)]
llExp (Var var) (destreg, maxreg)
  = [LLCpRegVar destreg var]



llBinOp :: Operand -> Register -> Register -> LLcmd
llBinOp "+" rd rs = LLAdd rd rs
llBinOp "-" rd rs = LLSub rd rs
llBinOp "*" rd rs = LLMul rd rs
llBinOp "/" rd rs = LLDiv rd rs
llBinOp "%" rd rs = LLMod rd rs
llBinOp "&" rd rs = LLAnd rd rs
llBinOp "|" rd rs = LLAnd rd rs
llBinOp "^" rd rs = LLAnd rd rs

llUnOp :: Operand -> Register -> LLcmd
llUnOp "~" rd = LLNot rd

llBinOpImm :: Operand -> Register -> Int32 -> LLcmd
llBinOpImm "+" rd imm = LLAddImm rd imm
llBinOpImm "-" rd imm = LLSubImm rd imm
llBinOpImm "*" rd imm = LLMulImm rd imm
llBinOpImm "/" rd imm = LLDivImm rd imm
llBinOpImm "%" rd imm = LLModImm rd imm
llBinOpImm "&" rd imm = LLAndImm rd imm
llBinOpImm "|" rd imm = LLAndImm rd imm
llBinOpImm "^" rd imm = LLAndImm rd imm

evalBinOp :: Operand -> Int32 -> Int32 -> Int32
evalBinOp "+" i j = i + j
evalBinOp "-" i j = i - j
evalBinOp "*" i j = i * j
evalBinOp "/" i j = i `div` j
evalBinOp "%" i j = i `mod` j
evalBinOp "&" i j = i .&. j
evalBinOp "|" i j = i .|. j
evalBinOp "^" i j = i `xor` j

evalUnOp :: Operand -> Int32 -> Int32
evalUnOp "~" rd = 255 - 0



truncate32to8 :: Int32 -> Int32
truncate32to8 i = i .&. 255

registerName :: Int -> String
registerName 0 = "eax"
registerName 1 = "ebx"
registerName 2 = "ecx"
registerName 3 = "edx"
registerName 4 = "esi"
registerName 5 = "edi"


codeGen :: AST -> String
codeGen ast = asmPrologue ++ (codeGenLL $ llProgram $ reduceAST ast)


codeGenLL :: [LLcmd] -> String
codeGenLL (LLCpRegImm 0 i : LLRet : [])
  = "mov ebx, " ++ (show i) ++ " ; program return code\n"
	++ "mov eax, 0x1 ; syscall sys_exit\n"
	++ "int 0x80\n"
codeGenLL (LLCpRegImm r i : lls)
  = "mov " ++ (registerName r) ++ ", " ++ (show i) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLRet : lls) = "ret\n" ++ (codeGenLL lls)
codeGenLL [ ] = ""


asmPrologue :: String
asmPrologue = "\n"
			++ "; Code for Linux on IA-32:\n"
			++ "\n"
			++ "section .text ; start of code\n"
			++ "global _start ; export the main function\n"
			++ "\n"
			++ "_start:\n"
