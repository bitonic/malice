module CodeGen 
       (
         codeGen,
       ) where

import Parser
import Data.Int (Int32)
import Data.Bits
import Data.Char
import Reduce

maxreg :: Register
maxreg = 4

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
  = (llStatlist statlist 0)



llStatlist :: StatementList -> Register -> [LLcmd]
llStatlist (s : ss) destreg
  = (llStat s destreg)
	++ (llStatlist ss destreg)
llStatlist [] _
  = []



llStat :: Statement -> Register -> [LLcmd]
llStat (Declare _ _) _
  = []
llStat (Assign var (Int imm)) _
  = [LLCpVarImm var imm]
llStat (Assign var exp1) destreg
  = (llExp exp1 destreg) ++ [(LLCpVarReg var destreg)]
llStat (Decrease var) destreg
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llStat (Increase var) destreg
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llStat (Return exp1) destreg
  = (llExp exp1 destreg) ++ [LLRet]



llExp :: Expr -> Register -> [LLcmd]
--llExp (BinOp op (Int imm1) (Int imm2)) destreg
--  = [LLCpRegImm destreg (evalBinOp op imm1 imm2)]
--llExp (BinOp op (Int imm) exp2) destreg
--  = (llExp exp2 destreg)
--	++ [llBinOpImm op destreg imm]
--llExp (BinOp op exp1 (Int imm)) destreg
--  = (llExp exp1 destreg)
--	++ [llBinOpImm op destreg imm]
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
  = [LLCpRegImm destreg (fromIntegral (ord c) :: Int32)]
llExp (Var var) destreg
  = [LLCpRegVar destreg var]



llBinOp :: Operand -> Register -> Register -> LLcmd
llBinOp "+" = LLAdd
llBinOp "-" = LLSub
llBinOp "*" = LLMul
llBinOp "/" = LLDiv
llBinOp "%" = LLMod
llBinOp "&" = LLAnd
llBinOp "|" = LLAnd
llBinOp "^" = LLAnd
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> Register -> LLcmd
llUnOp "~" = LLNot
llUnOp op = error ("llUnOp: Invalid operand encountered: " ++ op)

llBinOpImm :: Operand -> Register -> Int32 -> LLcmd
llBinOpImm "+" = LLAddImm
llBinOpImm "-" = LLSubImm
llBinOpImm "*" = LLMulImm
llBinOpImm "/" = LLDivImm
llBinOpImm "%" = LLModImm
llBinOpImm "&" = LLAndImm
llBinOpImm "|" = LLAndImm
llBinOpImm "^" = LLAndImm
llBinOpImm op = error ("llBinOpImm: Invalid operand encountered: " ++ op)


evalBinOp :: Operand -> Int32 -> Int32 -> Int32
evalBinOp "+" = (+)
evalBinOp "-" = (-)
evalBinOp "*" = (*)
evalBinOp "/" = div
evalBinOp "%" = mod
evalBinOp "&" = (.&.)
evalBinOp "|" = (.|.)
evalBinOp "^" = xor
evalBinOp op = error ("evalBinOp: Invalid operand encountered: " ++ op)

evalUnOp :: Operand -> Int32 -> Int32
evalUnOp "~" = (255 -)
evalUnOp op = error ("evalUnOp: Invalid operand encountered: " ++ op)



truncate32to8 :: Int32 -> Int32
truncate32to8 i = i .&. 255

registerName :: Int -> String
registerName 0 = "eax"
registerName 1 = "ebx"
registerName 2 = "ecx"
registerName 3 = "edx"
registerName 4 = "esi"
registerName 5 = "edi"
registerName r = error ("Error: Register index too high (" ++ (show r) ++ "). This really should not happen...")

codeGen :: AST -> String
codeGen ast = asmPrologue ++ (codeGenLL $ llProgram $ reduceAST ast)


codeGenLL :: [LLcmd] -> String
codeGenLL (LLCpRegImm 0 i : LLRet : [])
  = "mov ebx, " ++ (show (truncate32to8 i)) ++ " ; program return code\n"
	++ "mov eax, 0x1 ; syscall sys_exit\n"
	++ "int 0x80\n"
codeGenLL (LLCpRegImm r i : lls)
  = "mov " ++ (registerName r) ++ ", " ++ (show i) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLCpRegReg r1 r2 : lls)
  = "mov " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLAdd r1 r2 : lls)
  = "add " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLSub r1 r2 : lls)
  = "sub " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLMul r1 r2 : lls)
  = "imul " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
--codeGenLL (LLDiv r1 r2 : lls)
--  = "idiv " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
--	++ (codeGenLL lls)
--codeGenLL (LLMod r1 r2 : lls)
--  = "imod " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
--	++ (codeGenLL lls)
codeGenLL (LLAnd r1 r2 : lls)
  = "and " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLOr r1 r2 : lls)
  = "or " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLXOr r1 r2 : lls)
  = "xor " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLDec r : lls)
  = "dec " ++ (registerName r) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLInc r : lls)
  = "inc " ++ (registerName r) ++ "\n"
	++ (codeGenLL lls)
--codeGenLL (LLNot r : lls)
--  = "inc " ++ (registerName r) ++ "\n"
codeGenLL (LLAddImm r1 imm : lls)
  = "add " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLSubImm r1 imm : lls)
  = "sub " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLMulImm r1 imm : lls)
  = "imul " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls)
--codeGenLL (LLDivImm r1 imm : lls)
--  = "idiv " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
--	++ (codeGenLL lls)
--codeGenLL (LLModImm r1 imm : lls)
--  = "imod " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
--	++ (codeGenLL lls)
codeGenLL (LLAndImm r1 imm : lls)
  = "and " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLOrImm r1 imm : lls)
  = "or " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls)
codeGenLL (LLXOrImm r1 imm : lls)
  = "xor " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
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
