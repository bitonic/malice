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


cgLLParam :: LLParam -> SIM String
cgLLParam (PVar v) = do
  sym <- lookupSym v
  return $ case sym of
    Just (_, vid) -> "[ebp+" ++ (show (vid * (-4))) ++ "]"
    Nothing -> error $ "Variable " ++ v ++ " has not been defined (yet)."
cgLLParam (PReg r)
  = return $ registerName r
cgLLParam (PImm i)
  = return $ "dword " ++ (show i)
cgLLParam (PStr _)
  = error "cgLLParam: At this stage there should be no PStr left."
cgLLParam (PLbl l)
  = return l


cgLL2Param :: LLParam -> LLParam -> SIM String
cgLL2Param p1 p2 = do
  c1 <- cgLLParam p1
  c2 <- cgLLParam p2
  return $ c1 ++ ", " ++ c2

cgCmp :: String -> LLParam -> LLParam -> LLParam -> SIM String
cgCmp jt pdest p1 p2 = do
  d <- cgLLParam pdest
  parms <- cgLL2Param p1 p2
  lbl1 <- uniqLabel
  lbl2 <- uniqLabel
  return $ "cmp " ++ parms ++ "\n"
    ++ jt ++ " " ++ lbl1 ++ "\n"
    ++ "mov " ++ d ++ ", dword 0\n"
    ++ "jmp " ++ lbl2 ++ "\n"
    ++ lbl1 ++ ":\n"
    ++ "mov " ++ d ++ ", dword 1\n"
    ++ lbl2 ++ ":\n"


cgLine :: LLcmd -> SIM String
cgLine (LLDecl v _) = do
--cgLine (LLDecl v t) = do
  --sts <- getSymTabs
  --st <- popSymTab
  --pushSymTab $ M.insert v (t, sum $ map M.size sts) st
  initzero <- cgLine (LLCp (PVar v) (PImm 0))
  return initzero
cgLine (LLCp p1 p2) = do
  parms <- cgLL2Param p1 p2
  return $ "mov " ++ parms ++ "\n"
cgLine (LLAdd p1 p2) = do
  parms <- cgLL2Param p1 p2
  return $ "add " ++ parms++ "\n"
cgLine (LLSub p1 p2) = do
  parms <- cgLL2Param p1 p2
  return $ "sub " ++ parms ++ "\n"
cgLine (LLMul p1 p2) = do
  parms <- cgLL2Param p1 p2
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
  parms <- cgLL2Param p1 p2
  return $ "and " ++ parms ++ "\n"
cgLine (LLOr p1 p2) = do
  parms <- cgLL2Param p1 p2
  return $ "or " ++ parms ++ "\n"
cgLine (LLXOr p1 p2) = do
  parms <- cgLL2Param p1 p2
  return $ "xor " ++ parms ++ "\n"
cgLine (LLDec p1) = do
  parm <- cgLLParam p1
  return $  "dec " ++ parm ++ "\n"
cgLine (LLInc p1) = do
  parm <- cgLLParam p1
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
cgLine (LLClt p1 p2) = do
  c <- cgCmp "jl" p1 p1 p2
  return $ c
cgLine (LLCgt p1 p2) = do
  c <- cgCmp "jg" p1 p1 p2
  return $ c
cgLine (LLCle p1 p2) = do
  c <- cgCmp "jle" p1 p1 p2
  return $ c
cgLine (LLCge p1 p2) = do
  c <- cgCmp "jge" p1 p1 p2
  return $ c
cgLine (LLCeq p1 p2) = do
  c <- cgCmp "je" p1 p1 p2
  return $ c
cgLine (LLCneq p1 p2) = do
  c <- cgCmp "jne" p1 p1 p2
  return $ c
cgLine (LLCand p1 p2) = do
  parm1 <- cgLLParam p1
  parm2 <- cgLLParam p2
  lbl1 <- uniqLabel
  lbl2 <- uniqLabel
  return $ "cmp " ++ parm1 ++ ", dword 0\n"
    ++ "je " ++ lbl1 ++ "\n"
    ++ "cmp " ++ parm2 ++ ", dword 0\n"
    ++ "je " ++ lbl1 ++ "\n"
    ++ "mov " ++ parm1 ++ ", dword 1\n"
    ++ "jmp " ++ lbl2 ++ "\n"
    ++ lbl1 ++ ":\n"
    ++ "mov " ++ parm1 ++ ", dword 0\n"
    ++ lbl2 ++ ":\n"
cgLine (LLCor p1 p2) = do
  parm1 <- cgLLParam p1
  parm2 <- cgLLParam p2
  lbl1 <- uniqLabel
  lbl2 <- uniqLabel
  return $ "cmp " ++ parm1 ++ ", dword 0\n"
    ++ "jne " ++ lbl1 ++ "\n"
    ++ "cmp " ++ parm2 ++ ", dword 0\n"
    ++ "jne " ++ lbl1 ++ "\n"
    ++ "mov " ++ parm1 ++ ", dword 0\n"
    ++ "jmp " ++ lbl2 ++ "\n"
    ++ lbl1 ++ ":\n"
    ++ "mov " ++ parm1 ++ ", dword 1\n"
    ++ lbl2 ++ ":\n"
cgLine (LLRet) = do
  fn <- getFuncName
  return $ "jmp end_" ++ fn ++ "\n"
--  = return $ "ret\n"
cgLine (LLSpSub imm)
  = return $ "sub esp, " ++ (show imm) ++ "\n"
cgLine (LLSpAdd imm)
  = return $ "add esp, " ++ (show imm) ++ "\n"
cgLine (LLPush p1) = do
  parm <- cgLLParam p1
  return $ "push " ++ parm ++ "\n"
cgLine (LLPop p1) = do
  parm <- cgLLParam p1
  return $ "pop " ++ parm ++ "\n"
cgLine (LLDiv _ _)
  = error "cgLine: Impossible operand combination for LLDiv on i386"
cgLine (LLMod _ _)
  = error "cgLine: Impossible operand combination for LLMod on i386"
cgLine (LLNot _)
  = error "cgLine: Impossible operand combination for LLNot"
cgLine (LLSrcLine i)
  = return $ "\n; Source line " ++ (show i) ++ "\n"
cgLine (LLCall fn)
  = return $ "call " ++ fn ++ "\n"
cgLine (LLLabel l)
  = return $ l ++ ":\n"
cgLine (LLScope symtab ll) = do
  localsyt <- scanSymTab symtab
  cAlloc <- cgLine (LLSpSub $ fromIntegral (4 * (M.size symtab)))
  cDealloc  <- cgLine (LLSpAdd $ fromIntegral (4 * (M.size symtab)))
  pushSymTab localsyt
  c <- cgLL ll
  _ <- popSymTab
  return $ cAlloc ++ c ++ cDealloc
cgLine (LLJmp l)
  = return $ "jmp " ++ l ++ "\n"
cgLine (LLJmpZ l p1) = do
  parms <- cgLL2Param p1 p1
  return $ "test " ++ parms ++ "\n"
    ++ "je " ++ l ++ "\n"
cgLine (LLJmpNZ l p1) = do
  parms <- cgLL2Param p1 p1
  return $ "test " ++ parms ++ "\n"
    ++ "jne " ++ l ++ "\n"
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
              ++ "extern _print_string\n"
              ++ "extern _print_int\n"
              ++ "extern _read_int\n"




cgDA :: DeclarationAct -> SIM String
--cgDA (Function symtab name arglist rettype body)
cgDA (Function symtab name args _ body) = do
  putFuncName name
  localsyt <- scanSymTab symtab
  pushSymTab $ funcArgsSymTab args
  pushSymTab localsyt
  --pushSymTab M.empty
  putLabelCtr 0
  lBody <- llSL body
  cBody <- cgLL lBody -- have to do this first to calculate memory need
  cSave <- cgLL [LLPush (PReg 1)]
  cAlloc <- cgLine (LLSpSub $ fromIntegral (4 * (M.size symtab)))
  cDealloc  <- cgLine (LLSpAdd $ fromIntegral (4 * (M.size symtab)))
  cRestore <- cgLL [LLPop (PReg 1)]
  _ <- popSymTab
  _ <- popSymTab
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

cgD :: Declaration -> SIM String
--cgD (pos, (Function symtab name arglist rettype body))
cgD ( _, da ) = do
  cda <- cgDA da
  return cda



cgDL :: DeclarationList -> SIM String
cgDL dl = do
  cs <- mapM cgD dl
  stt <- getStrTab
  return $ asmPrologue
    ++ concat cs
    ++ "\n\n"
    ++ "section .data\n"
--    ++ concat [ "_str_" ++ (show i) ++ ": db \"" ++ s ++ "\",0\n" | (i, s) <- stt ]
    ++ concat [ "_str_" ++ (show i) ++ ": db " ++ (strToAsm s) ++ ",0\n" | (i, s) <- stt ]

cgAST :: AST -> String
cgAST (AST _ dl) = (((flip evalState) si).cgDL) dl
  where
    si = ("", [], [], (0, 0), 0)

