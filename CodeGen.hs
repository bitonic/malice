module CodeGen 
       (
         codeGen,
       ) where

import Parser
import Data.Int (Int32)
import Data.Bits
import Data.Char
import Reduce
import CodeCleanup

maxreg :: Register
maxreg = 4

-- Always from right to left, Intel-style

type Operand = String
type Register = Int
type Variable = String
type Immediate = Int32

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
     deriving (Show, Eq)




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
  = if ( (llRegsNeeded exp1) < (llRegsNeeded exp2) )
    then flipBinOpArgs ( BinOp op (sortExprWeight exp2) (sortExprWeight exp1) )
    else ( BinOp op (sortExprWeight exp1) (sortExprWeight exp2) )
sortExprWeight exp1
  = exp1

-- Evaluate complex expressions first, then variables and finally immediates
sortExprType :: Expr -> Expr
sortExprType (BinOp op (Int i) (Var v))
  = flipBinOpArgs ( BinOp op (sortExprType (Var v)) (sortExprType (Int i)) )
sortExprType (BinOp op (Int i) exp2)
  = flipBinOpArgs ( BinOp op (sortExprType (Int i)) (sortExprType exp2) )
sortExprType (BinOp op (Var v) exp2)
  = flipBinOpArgs ( BinOp op (sortExprType (Var v)) (sortExprType exp2) )
sortExprType exp1
  = exp1


--llExp (BinOp op (Int imm) exp2) destreg
--  = (llExp exp2 destreg)
--	++ [llBinOpImm op destreg imm]

-- Collapse immediates if possible
reduceExprImms :: Expr -> Expr
reduceExprImms (BinOp op (Int i1) (Int i2))
  = Int (evalBinOp op i1 i2)
reduceExprImms (BinOp op2 (BinOp op1 exp1 (Int i1)) (Int i2))
  | op1 == op2 = BinOp op1 (reduceExprImms (BinOp op1 exp1 (Int i1)))
                           (Int (evalBinOp op2 i1 i2))
  | otherwise = BinOp op1 (reduceExprImms (BinOp op1 exp1 (Int i1)))
                          (Int i2)
-- This does not scale for nested expressions that evaluate to an immediate.
reduceExprImms (UnOp op (Int i))
  = Int (evalUnOp op i)
reduceExprImms (UnOp op exp1)
  = UnOp op (reduceExprImms exp1)
reduceExprImms exp1
  = exp1


llRegsNeeded :: Expr -> Int
llRegsNeeded (BinOp _ exp1 exp2)
  = max (llRegsNeeded exp1) ((llRegsNeeded exp2) + 1)
llRegsNeeded (UnOp _ exp1)
  = llRegsNeeded exp1
llRegsNeeded (Int _)
  = 0
llRegsNeeded (Char _)
  = 0
llRegsNeeded (Var _)
  = 0


optimiseExpr :: Expr -> Expr
optimiseExpr exp1
  = reduceExprImms . sortExprType . sortExprWeight $ exp1



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
  = (llExp (optimiseExpr exp1) destreg) ++ [(LLCpVarReg var destreg)]
llStat (Decrease var) destreg
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llStat (Increase var) destreg
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llStat (Return exp1) destreg
  = (llExp (optimiseExpr exp1) destreg) ++ [LLRet]



llExp :: Expr -> Register -> [LLcmd]
--llExp (BinOp op (Int imm1) (Int imm2)) destreg
--  = [LLCpRegImm destreg (evalBinOp op imm1 imm2)]
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
  = [LLCpRegImm destreg (fromIntegral (ord c) :: Immediate)]
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

llBinOpImm :: Operand -> Register -> Immediate -> LLcmd
llBinOpImm "+" = LLAddImm
llBinOpImm "-" = LLSubImm
llBinOpImm "*" = LLMulImm
llBinOpImm "/" = LLDivImm
llBinOpImm "%" = LLModImm
llBinOpImm "&" = LLAndImm
llBinOpImm "|" = LLAndImm
llBinOpImm "^" = LLAndImm
llBinOpImm op = error ("llBinOpImm: Invalid operand encountered: " ++ op)


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

evalUnOp :: Operand -> Immediate -> Immediate
evalUnOp "~" = (255 -)
evalUnOp op = error ("evalUnOp: Invalid operand encountered: " ++ op)



truncate32to8 :: Immediate -> Immediate
truncate32to8 i = i .&. 255

registerName :: Int -> String
registerName 0 = "eax"
registerName 1 = "ebx"
registerName 2 = "ecx"
registerName 3 = "edx"
registerName 4 = "esi"
registerName 5 = "edi"
registerName r = error ("Error: Register index too high (" ++ (show r) ++ "). This really should not happen...")



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
codeGenLL (LLSpSub imm : lls)
  = "sub esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls)
codeGenLL (LLSpAdd imm : lls)
  = "add esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls)
codeGenLL [ ] = ""


asmPrologue :: String
asmPrologue = "\n"
			++ "; Code for Linux on IA-32:\n"
			++ "\n"
			++ "section .text ; start of code\n"
			++ "global _start ; export the main function\n"
			++ "\n"
			++ "_start:\n"

llAllocLocVars :: StatementList -> LLcmd
llAllocLocVars sl
  = LLSpSub $ fromIntegral (4 * (length $ getDecls $ sl))

llDeallocLocVars :: StatementList -> LLcmd
llDeallocLocVars sl
  = LLSpAdd $ fromIntegral (4 * (length . getDecls $ sl))

codeGen :: AST -> String
codeGen ast = asmPrologue
	++ (codeGenLL $ ((llAllocLocVars sl) : llsl) ++ [llDeallocLocVars sl])
            where
		rast@(Program sl) = reduceAST ast
		llsl = llProgram rast
