module CodeGen
       (
--         maliceCodeGen,
         codeGenDL,
         codeGenAST,
       ) where

import Common
import CGCommon
import LLGen
import qualified Data.Map as M
{-
import Control.Monad.State


type LLMonad = State (String, Map String Int, Int)

getFunName :: State String
getFunName = do
  (n, _, _) <- get
  return n

putFunName :: String -> State ()
putFunName n = do
  (_, m, i) <- get
  put (n, m, i) 
-}



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


codeLLParam :: ScopeInfo -> LLParam -> String
codeLLParam si (PVar v)
  = "[ebp-" ++ (show offset) ++ "]"
  where
    offset = scopevar * 4
    Just (_, scopevar) = (M.lookup v (getSymTab si))
codeLLParam _ (PReg r)
  = registerName r
codeLLParam _ (PImm i)
  = "dword " ++ (show i)


codeGenLine :: ScopeInfo -> LLcmd -> String
codeGenLine si (LLCp p1 p2)
  = "mov " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLAdd p1 p2)
  = "add " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLSub p1 p2)
  = "sub " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLMul p1 p2)
  = "imul " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine _ (LLDiv (PReg r1) (PReg r2))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", eax\n"
    ++ (if (r1 /= 3) then "pop edx\n" else "add esp, 4\n")
    ++ (if (r1 /= 0) then "pop eax\n" else "add esp, 4\n")
codeGenLine _ (LLMod (PReg r1) (PReg r2))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", edx\n"
    ++ (if (r1 /= 3) then "pop edx\n" else "add esp, 4\n")
    ++ (if (r1 /= 0) then "pop eax\n" else "add esp, 4\n")
codeGenLine si (LLDiv (PReg r1) (PImm imm))
  = codeGenLLsimple si [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLDiv (PReg r1) (PReg (succ r1))) ]
codeGenLine si (LLMod (PReg r1) (PImm imm))
  = codeGenLLsimple si [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLMod (PReg r1) (PReg (succ r1))) ]
codeGenLine si (LLAnd p1 p2)
  = "and " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLOr p1 p2)
  = "or " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLXOr p1 p2)
  = "xor " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
codeGenLine si (LLDec p1)
  = "dec " ++ (codeLLParam si p1) ++ "\n"
codeGenLine si (LLInc p1)
  = "inc " ++ (codeLLParam si p1) ++ "\n"
codeGenLine si (LLNot (PReg r))
  = codeGenLLsimple si [
      (LLCp (PReg (succ r)) (PReg r)),
      (LLCp (PReg r) (PImm 255)),
      (LLSub (PReg r) (PReg (succ r))) ]
--codeGenLine (LLNot (PVar v))
--  = codeGenLL [
--      (LLCp (PReg r) (PImm 255)),
--      (LLSub (PReg r) (PVar v)),
--      (LLCp (PVar v) (PReg r)) ]
codeGenLine _ (LLRet)
  = "ret\n"
codeGenLine _ (LLSpSub imm)
  = "sub esp, " ++ (show imm) ++ "\n"
codeGenLine _ (LLSpAdd imm)
  = "add esp, " ++ (show imm) ++ "\n"
codeGenLine si (LLPush p1)
  = "push " ++ (codeLLParam si p1) ++ "\n"
codeGenLine si (LLPop p1)
  = "pop " ++ (codeLLParam si p1) ++ "\n"
codeGenLine _ (LLDiv _ _)
  = error "codeGenLine: Impossible operand combination for LLDiv on i386"
codeGenLine _ (LLMod _ _)
  = error "codeGenLine: Impossible operand combination for LLMod on i386"
codeGenLine _ (LLNot _)
  = error "codeGenLine: Impossible operand combination for LLNot"
codeGenLine _ (LLSrcLine i)
  = "; Source line " ++ (show i) ++ "\n"
codeGenLine _ (LLPrint (PStr str))
  = error "Implement CG Print"
codeGenLine _ (LLCall fn)
  = "call " ++ fn ++ "\n"
--codeGenLine _
--  = error "codeGenLine: Unknown operator/operand combination"


codeGenLLsimple :: ScopeInfo -> [LLcmd] -> String
codeGenLLsimple si = concat . map (codeGenLine si)



--            body                    asm
codeGenLL :: ScopeInfo -> [LLcmd] -> String
codeGenLL si (LLRet : ls)
  = "jmp end_" ++ (getFunName si) ++ "\n"
    ++ codeGenLL si ls
codeGenLL si (l:ls)
  = codeGenLine si l
    ++ codeGenLL si ls
codeGenLL _ []
  = "mov eax, 0\n"


asmPrologue :: String
asmPrologue = "\n"
              ++ "; Code for Linux on IA-32:\n"
              ++ "\n"
              ++ "section .text ; start of code\n\n"



--maliceCodeGen :: StatementList -> VarTypes -> String
--maliceCodeGen sl vt = asmPrologue ++ codeGenLL llsl ++ globs
--  where
--    llsl = maliceLL sl
--    globs = "\n\nsection .data ; global variables go here\n" ++ M.foldWithKey codeGenGlobVar "" vt

prepSymTabOffsets' :: Int -> [(Variable, (MaliceType, Int))] ->  [(Variable, (MaliceType, Int))]
prepSymTabOffsets' _ []
  = []
prepSymTabOffsets' num ( (v, (t, _)) : ss )
  = (v, (t, num)) : prepSymTabOffsets' (succ num) ss

prepSymTabOffsets :: SymbolTable -> SymbolTable
prepSymTabOffsets = M.fromList . (prepSymTabOffsets' 0) . M.toAscList


codeGenDA :: DeclarationAct -> String
--codeGenDA (Function symtab name arglist rettype body)
codeGenDA (Function symtab name _ _ body)
  = "\n"
    ++ "global " ++ name ++ "\n"
    ++ name ++ ":\n\n"
    ++ codeGenLLsimple si llSave
    ++ "push ebp\n"
    ++ "mov ebp, esp\n"
    ++ codeGenLine si llAlloc
    ++ "\n"
    ++ codeGenLL si (maliceLL body)
    ++ "\n"
    ++ "end_" ++ name ++ ":\n"
    ++ codeGenLine si llDealloc
    ++ "pop ebp\n" 
    ++ codeGenLLsimple si llRestore
    ++ "ret\n"
  where
    llAlloc = (LLSpSub $ fromIntegral (4 * (M.size symtab)))
    llDealloc = (LLSpAdd $ fromIntegral (4 * (M.size symtab)))
    llSave = [LLPush (PReg 1)]
    llRestore = [LLPop (PReg 1)]
    si = (name, (prepSymTabOffsets symtab))

codeGenD :: Declaration -> String
codeGenD (_, da) = codeGenDA da


codeGenDL :: DeclarationList -> String
codeGenDL dl
  = asmPrologue
    ++ concat (map codeGenD dl)
    ++ "\n"

codeGenAST :: AST -> String
codeGenAST (AST _ dl) = codeGenDL dl

