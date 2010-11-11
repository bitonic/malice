module CodeGen
       (
         maliceCodeGen,
       ) where

import LLGen
import Parser
import Semantics
import qualified Data.Map as M

--maxreg :: Register
--maxreg = 4

-- Always from right to left, Intel-style

registerName :: Int -> String
registerName 0 = "eax"
registerName 1 = "ebx"
registerName 2 = "ecx"
registerName 3 = "edx"
registerName 4 = "esi"
registerName 5 = "edi"
registerName r = error ("Error: Register index too high (" ++ (show r) ++ "). This really should not happen...")



codeGenLL :: [LLcmd] -> String
--codeGenLL (LLCpRegImm 0 i : LLRet : []) _
--  = "mov ebx, " ++ (show (truncate32to8 i)) ++ " ; program return code\n"
--	++ "mov eax, 0x1 ; syscall sys_exit\n"
--	++ "int 0x80\n"
codeGenLL (LLCpRegVar r v : lls)
  = "mov " ++ (registerName r) ++ ", [" ++ v ++ "]\n"
    ++ (codeGenLL lls)
codeGenLL (LLCpVarReg v r : lls)
  = "mov [" ++ v ++ "], " ++ (registerName r) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLCpVarImm v imm : lls)
  = "mov [" ++ v ++ "], dword " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
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
codeGenLL (LLDiv r1 r2 : lls)
-- WARNING: If r1 == 0 and r2 == 3 we might have loss of information...
  = "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, " ++ (registerName r2) ++ "\n"
    ++ "idiv " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", eax\n"
    ++ "pop edx"
    ++ "pop eax"
    ++ (codeGenLL lls)
codeGenLL (LLMod r1 r2 : lls)
-- WARNING: If r1 == 0 and r2 == 3 we might have loss of information...
  = "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, " ++ (registerName r2) ++ "\n"
    ++ "idiv " ++ (registerName r1) ++ ", " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", edx\n"
    ++ "pop edx"
    ++ "pop eax"
    ++ (codeGenLL lls)
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
codeGenLL (LLNot r : lls)
  = codeGenLL ( (LLCpRegReg (r+1) r) : (LLCpRegImm r 256) : (LLSub r (r+1))
                : lls )
codeGenLL (LLAddImm r1 imm : lls)
  = "add " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLSubImm r1 imm : lls)
  = "sub " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLMulImm r1 imm : lls)
  = "imul " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLDivImm r1 imm : lls)
  = codeGenLL ( (LLCpRegImm (r1+1) imm) : (LLDiv r1 (r1+1)) : lls )
codeGenLL (LLModImm r1 imm : lls)
  = codeGenLL ( (LLCpRegImm (r1+1) imm) : (LLMod r1 (r1+1)) : lls )
codeGenLL (LLAndImm r1 imm : lls)
  = "and " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLOrImm r1 imm : lls)
  = "or " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLXOrImm r1 imm : lls)
  = "xor " ++ (registerName r1) ++ ", " ++ (show imm) ++ "\n"
    ++ (codeGenLL lls)
codeGenLL (LLRet : lls)
  = "ret\n" ++ (codeGenLL lls)
codeGenLL (LLSpSub imm : lls)
  = "sub esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls)
codeGenLL (LLSpAdd imm : lls)
  = "add esp, " ++ (show imm) ++ "\n" ++ (codeGenLL lls)
codeGenLL [ ]
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

{-
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
-}

codeGenGlobVar :: Variable -> MaliceType -> String -> String
codeGenGlobVar v _ rest
  = v ++ " DD 0\n" ++ rest

maliceCodeGen :: StatementList -> VarTypes -> String
maliceCodeGen sl vt = asmPrologue ++ codeGenLL llsl ++ globs
  where
    llsl = maliceLL sl
    globs = "\n\nsection .data ; global variables go here\n" ++ M.foldWithKey codeGenGlobVar "" vt