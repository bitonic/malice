module CodeGen
       (
--         maliceCodeGen,
         codeGenDL,
         codeGenAST,
       ) where

import Common
import LLGen
{-
--import Control.Monad.State
--import qualified Data.Map as M

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


codeLLParam :: LLParam -> String
codeLLParam (PVar v)
  = "[" ++ v ++ "]"
codeLLParam (PReg r)
  = registerName r
codeLLParam (PImm i)
  = "dword " ++ (show i)


codeGenLine :: LLcmd -> String
codeGenLine (LLCp p1 p2)
  = "mov " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLAdd p1 p2)
  = "add " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLSub p1 p2)
  = "sub " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLMul p1 p2)
  = "imul " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLDiv (PReg r1) (PReg r2))
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
codeGenLine (LLMod (PReg r1) (PReg r2))
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
codeGenLine (LLDiv (PReg r1) (PImm imm))
  = codeGenLLsimple [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLDiv (PReg r1) (PReg (succ r1))) ]
codeGenLine (LLMod (PReg r1) (PImm imm))
  = codeGenLLsimple [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLMod (PReg r1) (PReg (succ r1))) ]
codeGenLine (LLAnd p1 p2)
  = "and " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLOr p1 p2)
  = "or " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLXOr p1 p2)
  = "xor " ++ (codeLLParam p1) ++ ", " ++ (codeLLParam p2) ++ "\n"
codeGenLine (LLDec p1)
  = "dec " ++ (codeLLParam p1) ++ "\n"
codeGenLine (LLInc p1)
  = "inc " ++ (codeLLParam p1) ++ "\n"
codeGenLine (LLNot (PReg r))
  = codeGenLLsimple [
      (LLCp (PReg (succ r)) (PReg r)),
      (LLCp (PReg r) (PImm 255)),
      (LLSub (PReg r) (PReg (succ r))) ]
--codeGenLine (LLNot (PVar v))
--  = codeGenLL [
--      (LLCp (PReg r) (PImm 255)),
--      (LLSub (PReg r) (PVar v)),
--      (LLCp (PVar v) (PReg r)) ]
codeGenLine (LLRet)
  = "ret\n"
codeGenLine (LLSpSub imm)
  = "sub esp, " ++ (show imm) ++ "\n"
codeGenLine (LLSpAdd imm)
  = "add esp, " ++ (show imm) ++ "\n"
codeGenLine (LLPush p1)
  = "push " ++ (codeLLParam p1) ++ "\n"
codeGenLine (LLPop p1)
  = "pop " ++ (codeLLParam p1) ++ "\n"
codeGenLine (LLDiv _ _)
  = error "codeGenLine: Impossible operand combination for LLDiv on i386"
codeGenLine (LLMod _ _)
  = error "codeGenLine: Impossible operand combination for LLMod on i386"
codeGenLine (LLNot _)
  = error "codeGenLine: Impossible operand combination for LLNot"
codeGenLine (LLSrcLine i)
  = "; Source line " ++ (show i) ++ "\n"
--codeGenLine _
--  = error "codeGenLine: Unknown operator/operand combination"


codeGenLLsimple :: [LLcmd] -> String
codeGenLLsimple lls = concat $ map codeGenLine lls


getFunName :: (String) -> String
getFunName (fn) = fn

--            body       f-name      asm
codeGenLL :: [LLcmd] -> (String) -> String
codeGenLL (LLRet : ls) ed
  = "jmp end_" ++ (getFunName ed) ++ "\n"
    ++ codeGenLL ls ed
codeGenLL (l:ls) ed
  = codeGenLine l
    ++ codeGenLL ls ed
codeGenLL [] _
  = "mov eax, 0\n"


asmPrologue :: String
asmPrologue = "\n"
              ++ "; Code for Linux on IA-32:\n"
              ++ "\n"
              ++ "section .text ; start of code\n"
              ++ "global _start ; export the main function\n"
              ++ "\n"
              ++ "_start:\n"
              ++ "call " ++ mainFunction ++ "\n"
              ++ "mov ebx, eax\n"
              ++ "mov eax, 1\n"
              ++ "int 0x80\n"
              ++ "\n\n"

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

--maliceCodeGen :: StatementList -> VarTypes -> String
--maliceCodeGen sl vt = asmPrologue ++ codeGenLL llsl ++ globs
--  where
--    llsl = maliceLL sl
--    globs = "\n\nsection .data ; global variables go here\n" ++ M.foldWithKey codeGenGlobVar "" vt


codeGenDA :: DeclarationAct -> String
--codeGenDA (Function symtab name arglist rettype body)
codeGenDA (Function _ name _ _ body)
  = "\n"
    ++ "global " ++ name ++ "\n"
    ++ name ++ ":\n\n"
    ++ codeGenLL (maliceLL body) name
    ++ "\n"
    ++ "end_" ++ name ++ ":\n"
    ++ "ret"
    ++ "\n"

codeGenD :: Declaration -> String
codeGenD (_, da) = codeGenDA da


codeGenDL :: DeclarationList -> String
codeGenDL dl
  = asmPrologue
    ++ concat (map codeGenD dl)
    ++ "\n"

codeGenAST :: AST -> String
codeGenAST (AST _ dl) = codeGenDL dl

