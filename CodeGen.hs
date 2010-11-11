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

--maxreg :: Register
--maxreg = 4

-- Always from right to left, Intel-style

type Operand = String
type Register = Int
type Variable = String
type Immediate = Int32

type VarMap = [(Variable, String)]

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
	deriving (Show, Eq)




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
  = if ( (exprIntermeds exp1) < (exprIntermeds exp2) )
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


reduceExprImms' :: Expr -> Expr
reduceExprImms' (BinOp op (Int i1) (Int i2))
  = Int (evalBinOp op i1 i2)
reduceExprImms' (UnOp op (Int i))
  = Int (evalUnOp op i)
reduceExprImms' exp1
  = exp1

-- Collapse immediates from right to left if possible
reduceExprImms :: Expr -> Expr
reduceExprImms (BinOp op2 e1@(BinOp op1 exp1 (Int i1)) e2@(Int i2))
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


optimiseExpr :: Expr -> Expr
optimiseExpr exp1
  = reduceExprImms . sortExprType . sortExprWeight $ exp1





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



codeGenLL :: [LLcmd] -> VarMap -> String
--codeGenLL (LLCpRegImm 0 i : LLRet : []) _
--  = "mov ebx, " ++ (show (truncate32to8 i)) ++ " ; program return code\n"
--	++ "mov eax, 0x1 ; syscall sys_exit\n"
--	++ "int 0x80\n"
codeGenLL (LLCpRegVar r v : lls) varmap
  = "mov " ++ (registerName r) ++ ", " ++ (lookupVar v varmap) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLCpVarReg v r : lls) varmap
  = "mov " ++ (lookupVar v varmap) ++ ", " ++ (registerName r) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLCpVarImm v imm : lls) varmap
  = "mov " ++ (lookupVar v varmap) ++ ", dword " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLCpRegImm r i : lls) varmap
  = "mov " ++ (registerName r) ++ ", " ++ (show i) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLCpRegReg r1 r2 : lls) varmap
  = "mov " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLAdd r1 r2 : lls) varmap
  = "add " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLSub r1 r2 : lls) varmap
  = "sub " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLMul r1 r2 : lls) varmap
  = "imul " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLDiv r1 r2 : lls) varmap
-- WARNING: If r1 == 0 and r2 == 3 we might have loss of information...
  = "push eax\n"
	++ "push edx\n"
	++ "mov eax, " ++ (registerName r1) ++ "\n"
	++ "mov edx, " ++ (registerName r2) ++ "\n"
	++ "idiv " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ "mov " ++ (registerName r1) ++ ", eax\n"
	++ "pop edx"
	++ "pop eax"
	++ (codeGenLL lls varmap)
codeGenLL (LLMod r1 r2 : lls) varmap
-- WARNING: If r1 == 0 and r2 == 3 we might have loss of information...
  = "push eax\n"
	++ "push edx\n"
	++ "mov eax, " ++ (registerName r1) ++ "\n"
	++ "mov edx, " ++ (registerName r2) ++ "\n"
	++ "idiv " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ "mov " ++ (registerName r1) ++ ", edx\n"
	++ "pop edx"
	++ "pop eax"
	++ (codeGenLL lls varmap)
codeGenLL (LLAnd r1 r2 : lls) varmap
  = "and " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLOr r1 r2 : lls) varmap
  = "or " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLXOr r1 r2 : lls) varmap
  = "xor " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLDec r : lls) varmap
  = "dec " ++ (registerName r) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLInc r : lls) varmap
  = "inc " ++ (registerName r) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLNot r : lls) varmap
  = codeGenLL ( (LLCpRegReg (r+1) r) : (LLCpRegImm r 256) : (LLSub r (r+1))
		: lls ) varmap
codeGenLL (LLAddImm r1 imm : lls) varmap
  = "add " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLSubImm r1 imm : lls) varmap
  = "sub " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLMulImm r1 imm : lls) varmap
  = "imul " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLDivImm r1 imm : lls) varmap
  = codeGenLL ( (LLCpRegImm (r1+1) imm) : (LLDiv r1 (r1+1)) : lls ) varmap
codeGenLL (LLModImm r1 imm : lls) varmap
  = codeGenLL ( (LLCpRegImm (r1+1) imm) : (LLMod r1 (r1+1)) : lls ) varmap
codeGenLL (LLAndImm r1 imm : lls) varmap
  = "and " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLOrImm r1 imm : lls) varmap
  = "or " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLXOrImm r1 imm : lls) varmap
  = "xor " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
	++ (codeGenLL lls varmap)
codeGenLL (LLRet : lls) varmap
  = "ret\n" ++ (codeGenLL lls varmap)
codeGenLL (LLSpSub imm : lls) varmap
  = "sub esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls varmap)
codeGenLL (LLSpAdd imm : lls) varmap
  = "add esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls varmap)
codeGenLL [ ] _
  = ""


asmPrologue :: String
asmPrologue = "\n"
			++ "; Code for Linux on IA-32:\n"
			++ "\n"
			++ "section .text ; start of code\n"
			++ "global _start ; export the main function\n"
			++ "\n"
			++ "_start:\n"
			++ "call main\n"
			++ "mov ebx, eax\n"
			++ "mov eax, 1\n"
			++ "int 0x80\n"
			++ "\n\n"
			++ "main:\n"

llAllocLocVars :: StatementList -> LLcmd
llAllocLocVars sl
  = LLSpSub $ fromIntegral (4 * (length $ getDecls $ sl))

llDeallocLocVars :: StatementList -> LLcmd
llDeallocLocVars sl
  = LLSpAdd $ fromIntegral (4 * (length . getDecls $ sl))

getVarMap :: StatementList -> Int -> VarMap
getVarMap (Declare _ x : sl) offset
  = (x, "[esp+" ++ (show offset) ++ "]") : getVarMap sl (offset + 4)
getVarMap (_ : sl) offset
  = getVarMap sl offset
getVarMap [] _
  = []

lookupVar :: Variable -> VarMap -> String
lookupVar x varmap
  | (length varass) == 1 = head varass
  | otherwise = error "Error: Trying to access a variable that has not been defined yet or more than once."
  where
    varass = [ str | (var, str) <- varmap, var == x ] 

fiddleDealloc :: [LLcmd] -> StatementList -> [LLcmd]
fiddleDealloc (LLRet : lls) sl
  = (llDeallocLocVars sl) : (LLRet : (fiddleDealloc lls sl))
fiddleDealloc (ls : lls) sl
  = ls : (fiddleDealloc lls sl)
fiddleDealloc [ ] _
  = [ ]

codeGen :: StatementList -> String
codeGen sl = asmPrologue
	++ (flip (codeGenLL) varmap $ ((llAllocLocVars sl) : (fiddleDealloc llsl sl)))
            where
		--rsl = maliceReduce sl
		rsl = sl
		llsl = llStatlist rsl 0
		varmap = getVarMap sl 0
