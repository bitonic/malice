module CodeGen
       (
         cgDL,
         cgAST,
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


cgLine :: ScopeInfo -> LLcmd -> String
cgLine si (LLCp p1 p2)
  = "mov " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLAdd p1 p2)
  = "add " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLSub p1 p2)
  = "sub " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLMul p1 p2)
  = "imul " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine _ (LLDiv (PReg r1) (PReg r2))
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
cgLine _ (LLMod (PReg r1) (PReg r2))
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
cgLine si (LLDiv (PReg r1) (PImm imm))
  = cgLLsimple si [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLDiv (PReg r1) (PReg (succ r1))) ]
cgLine si (LLMod (PReg r1) (PImm imm))
  = cgLLsimple si [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLMod (PReg r1) (PReg (succ r1))) ]
cgLine si (LLAnd p1 p2)
  = "and " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLOr p1 p2)
  = "or " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLXOr p1 p2)
  = "xor " ++ (codeLLParam si p1) ++ ", " ++ (codeLLParam si p2) ++ "\n"
cgLine si (LLDec p1)
  = "dec " ++ (codeLLParam si p1) ++ "\n"
cgLine si (LLInc p1)
  = "inc " ++ (codeLLParam si p1) ++ "\n"
cgLine si (LLNot (PReg r))
  = cgLLsimple si [
      (LLCp (PReg (succ r)) (PReg r)),
      (LLCp (PReg r) (PImm 255)),
      (LLSub (PReg r) (PReg (succ r))) ]
--cgLine (LLNot (PVar v))
--  = cgLL [
--      (LLCp (PReg r) (PImm 255)),
--      (LLSub (PReg r) (PVar v)),
--      (LLCp (PVar v) (PReg r)) ]
cgLine _ (LLRet)
  = "ret\n"
cgLine _ (LLSpSub imm)
  = "sub esp, " ++ (show imm) ++ "\n"
cgLine _ (LLSpAdd imm)
  = "add esp, " ++ (show imm) ++ "\n"
cgLine si (LLPush p1)
  = "push " ++ (codeLLParam si p1) ++ "\n"
cgLine si (LLPop p1)
  = "pop " ++ (codeLLParam si p1) ++ "\n"
cgLine _ (LLDiv _ _)
  = error "cgLine: Impossible operand combination for LLDiv on i386"
cgLine _ (LLMod _ _)
  = error "cgLine: Impossible operand combination for LLMod on i386"
cgLine _ (LLNot _)
  = error "cgLine: Impossible operand combination for LLNot"
cgLine _ (LLSrcLine i)
  = "; Source line " ++ (show i) ++ "\n"
cgLine _ (LLPrint (PStr str))
  = error "Implement CG Print"
cgLine _ (LLCall fn)
  = "call " ++ fn ++ "\n"
--cgLine _
--  = error "cgLine: Unknown operator/operand combination"


cgLLsimple :: ScopeInfo -> [LLcmd] -> String
cgLLsimple si = concat . map (cgLine si)



--            body                    asm
cgLL :: ScopeInfo -> [LLcmd] -> String
cgLL si (LLRet : ls)
  = "jmp end_" ++ (getFunName si) ++ "\n"
    ++ cgLL si ls
cgLL si (l:ls)
  = cgLine si l
    ++ cgLL si ls
cgLL _ []
  = "mov eax, 0\n"


asmPrologue :: String
asmPrologue = "\n"
              ++ "; Code for Linux on IA-32:\n"
              ++ "\n"
              ++ "section .text ; start of code\n\n"



--maliceCodeGen :: StatementList -> VarTypes -> String
--maliceCodeGen sl vt = asmPrologue ++ cgLL llsl ++ globs
--  where
--    llsl = maliceLL sl
--    globs = "\n\nsection .data ; global variables go here\n" ++ M.foldWithKey cgGlobVar "" vt

prepSymTabOffsets' :: Int -> [(Variable, (MaliceType, Int))] ->  [(Variable, (MaliceType, Int))]
prepSymTabOffsets' _ []
  = []
prepSymTabOffsets' num ( (v, (t, _)) : ss )
  = (v, (t, num)) : prepSymTabOffsets' (succ num) ss

prepSymTabOffsets :: SymbolTable -> SymbolTable
prepSymTabOffsets = M.fromList . (prepSymTabOffsets' 0) . M.toAscList



cgDA :: DeclarationAct -> String
--cgDA (Function symtab name arglist rettype body)
cgDA (Function symtab name _ _ body)
  = "\n"
    ++ "global " ++ name ++ "\n"
    ++ name ++ ":\n\n"
    ++ cgLLsimple si llSave
    ++ "push ebp\n"
    ++ "mov ebp, esp\n"
    ++ cgLine si llAlloc
    ++ "\n"
    ++ cgLL si (llSL si body)
    ++ "\n"
    ++ "end_" ++ name ++ ":\n"
    ++ cgLine si llDealloc
    ++ "pop ebp\n" 
    ++ cgLLsimple si llRestore
    ++ "ret\n"
  where
    llAlloc = (LLSpSub $ fromIntegral (4 * (M.size symtab)))
    llDealloc = (LLSpAdd $ fromIntegral (4 * (M.size symtab)))
    llSave = [LLPush (PReg 1)]
    llRestore = [LLPop (PReg 1)]
    si = (name, (prepSymTabOffsets symtab), [])

cgD :: Declaration -> String
cgD (_, da) = cgDA da


cgDL :: DeclarationList -> String
cgDL dl
  = asmPrologue
    ++ concat (map cgD dl)
    ++ "\n"

cgAST :: AST -> String
cgAST (AST _ dl) = cgDL dl

