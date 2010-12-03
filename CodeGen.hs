module CodeGen
       (
         cgDL,
         cgAST,
       ) where

import Common
import CGCommon
import LLGen
import qualified Data.Map as M
import Control.Monad.State

{-
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


codeLLParam :: LLParam -> SIM String
codeLLParam (PVar v) = do
  si <- get
  return $ case M.lookup v (getSymTab si) of
    Just (_, id) -> "[ebp-" ++ (show (id * 4)) ++ "]"
    Nothing -> error "Variable " ++ v ++ " has not been defined (yet)."
codeLLParam (PReg r)
  = return $ registerName r
codeLLParam (PImm i)
  = return $ "dword " ++ (show i)


codeLL2Param :: LLParam -> LLParam -> SIM String
codeLL2Param p1 p2 = do
  c1 <- codeLLParam p1
  c2 <- codeLLParam p2
  return $ c1 ++ ", " ++ c2


cgLine :: LLcmd -> SIM String
cgLine (LLCp p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "mov " ++ parms ++ "\n"
cgLine (LLAdd p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "add " ++ parms++ "\n"
cgLine (LLSub p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "sub " ++ parms ++ "\n"
cgLine (LLMul p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "imul " ++ parms ++ "\n"
cgLine (LLDiv (PReg r1) (PReg r2))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = return $ "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", eax\n"
    ++ (if (r1 /= 3) then "pop edx\n" else "add esp, 4\n")
    ++ (if (r1 /= 0) then "pop eax\n" else "add esp, 4\n")
cgLine (LLMod (PReg r1) (PReg r2))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = return $ "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ (registerName r1) ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ (registerName r2) ++ "\n"
    ++ "mov " ++ (registerName r1) ++ ", edx\n"
    ++ (if (r1 /= 3) then "pop edx\n" else "add esp, 4\n")
    ++ (if (r1 /= 0) then "pop eax\n" else "add esp, 4\n")
cgLine (LLDiv (PReg r1) (PImm imm)) = do
  c <- cgLL [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLDiv (PReg r1) (PReg (succ r1))) ]
  return c
cgLine (LLMod (PReg r1) (PImm imm)) = do
  c <- cgLL [
      (LLCp (PReg (succ r1)) (PImm imm)),
      (LLMod (PReg r1) (PReg (succ r1))) ]
  return c
cgLine (LLAnd p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "and " ++ parms ++ "\n"
cgLine (LLOr p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "or " ++ parms ++ "\n"
cgLine (LLXOr p1 p2) = do
  parms <- codeLL2Param p1 p2
  return $ "xor " ++ parms ++ "\n"
cgLine (LLDec p1) = do
  parm <- codeLLParam p1
  return $  "dec " ++ parm ++ "\n"
cgLine (LLInc p1) = do
  parm <- codeLLParam p1
  return $ "inc " ++ parm ++ "\n"
cgLine (LLNot (PReg r)) = do
  c <- cgLL [
      (LLCp (PReg (succ r)) (PReg r)),
      (LLCp (PReg r) (PImm 255)),
      (LLSub (PReg r) (PReg (succ r))) ]
  return c
--cgLine (LLNot (PVar v))
--  = cgLL [
--      (LLCp (PReg r) (PImm 255)),
--      (LLSub (PReg r) (PVar v)),
--      (LLCp (PVar v) (PReg r)) ]
cgLine (LLRet) = do
  si <- get
  return $ "jmp end_" ++ (getFunName si) ++ "\n"
--  = return $ "ret\n"
cgLine (LLSpSub imm)
  = return $ "sub esp, " ++ (show imm) ++ "\n"
cgLine (LLSpAdd imm)
  = return $ "add esp, " ++ (show imm) ++ "\n"
cgLine (LLPush p1) = do
  parm <- codeLLParam p1
  return $ "push " ++ parm ++ "\n"
cgLine (LLPop p1) = do
  parm <- codeLLParam p1
  return $ "pop " ++ parm ++ "\n"
cgLine (LLDiv _ _)
  = error "cgLine: Impossible operand combination for LLDiv on i386"
cgLine (LLMod _ _)
  = error "cgLine: Impossible operand combination for LLMod on i386"
cgLine (LLNot _)
  = error "cgLine: Impossible operand combination for LLNot"
cgLine (LLSrcLine i)
  = return $ "; Source line " ++ (show i) ++ "\n"
cgLine (LLPrint (PStr str))
  = error "Implement CG Print"
cgLine (LLCall fn)
  = return $ "call " ++ fn ++ "\n"
--cgLine _
--  = error "cgLine: Unknown operator/operand combination"


cgLL :: [LLcmd] -> SIM String
cgLL lls = do
  c <- mapM cgLine lls
  return $ concat c




asmPrologue :: String
asmPrologue = "\n"
              ++ "; Code for Linux on IA-32:\n"
              ++ "\n"
              ++ "section .text ; start of code\n\n"



prepSymTabOffsets' :: Int -> [(Variable, (MaliceType, Int))] ->  [(Variable, (MaliceType, Int))]
prepSymTabOffsets' _ []
  = []
prepSymTabOffsets' num ( (v, (t, _)) : ss )
  = (v, (t, num)) : prepSymTabOffsets' (succ num) ss

prepSymTabOffsets :: SymbolTable -> SymbolTable
prepSymTabOffsets = M.fromList . (prepSymTabOffsets' 0) . M.toAscList



cgDA :: DeclarationAct -> SIM String
--cgDA (Function symtab name arglist rettype body)
cgDA (Function symtab name _ _ body) = do
  si <- get
  lBody <- llSL body
  cSave <- cgLL llSave
  cAlloc <- cgLine llAlloc
  cBody <- cgLL lBody
  cDealloc  <- cgLine  llDealloc
  cRestore <- cgLL llRestore
  return $ "\n"
    ++ "global " ++ name ++ "\n"
    ++ name ++ ":\n\n"
    ++ cSave
    ++ "push ebp\n"
    ++ "mov ebp, esp\n"
    ++ cAlloc
    ++ "\n"
    ++ cBody
    ++ "mov eax, 0\n"
    ++ "\n"
    ++ "end_" ++ name ++ ":\n"
    ++ cDealloc
    ++ "pop ebp\n" 
    ++ cRestore
    ++ "ret\n"
  where
    llAlloc = (LLSpSub $ fromIntegral (4 * (M.size symtab)))
    llDealloc = (LLSpAdd $ fromIntegral (4 * (M.size symtab)))
    llSave = [LLPush (PReg 1)]
    llRestore = [LLPop (PReg 1)]

cgD :: Declaration -> String
--cgD (pos, (Function symtab name arglist rettype body))
cgD ( _, da@(Function symtab name _ _ body) )
  = (((flip evalState) si).cgDA) da
  where
    si = (name, (prepSymTabOffsets symtab), [])



cgDL :: DeclarationList -> String
cgDL dl
  = asmPrologue
    ++ concat (map cgD dl)
    ++ "\n"

cgAST :: AST -> String
cgAST (AST _ dl) = cgDL dl

