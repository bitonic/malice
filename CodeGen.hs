module CodeGen
       (
         cgDL,
         cgAST,
       ) where

import Common
import CGCommon
import LLGen
--import qualified Data.Map as M
import Control.Monad.State


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
registerName r = error ("Error: Register index too high ("
                        ++ show r ++ "). This really should not happen...")


cgLLparam :: LLparam -> SIM String
cgLLparam (Pvar v) = do
  sym <- lookupSym v
  return $ case sym of
    Just (_, vid) -> "dword [ebp+" ++ show (vid * (-4)) ++ "]"
    Nothing -> error $ "Variable " ++ v ++ " has not been defined (yet)."
cgLLparam (Preg r)
  = return $ registerName r
cgLLparam (Pimm i)
  = return $ "dword " ++ show i
cgLLparam (Plbl l)
  = return l
cgLLparam (Pderef p1 (Pimm i)) = do -- only with registers and immediates
  c1 <-cgLLparam p1
  return $ "[" ++ c1 ++ "+4*" ++ show i ++ "]"
cgLLparam (Pderef p1 p2) = do -- only with registers and registers
  c1 <- cgLLparam p1
  c2 <- cgLLparam p2
  return $ "[" ++ c1 ++ "+4*" ++ c2 ++ "]"


cgLL2param :: LLparam -> LLparam -> SIM String
cgLL2param p1 p2 = do
  c1 <- cgLLparam p1
  c2 <- cgLLparam p2
  return $ c1 ++ ", " ++ c2


cgLLparams :: LLparams -> SIM String
cgLLparams  Zero       = return ""
cgLLparams (Two p1 p2) = cgLL2param p1 p2
cgLLparams (One p)     = cgLLparam p


cgCmp :: String -> LLparam -> LLparam -> LLparam -> SIM String
cgCmp jt pdest p1 p2 = do
  d <- cgLLparam pdest
  parms <- cgLL2param p1 p2
  lbl1 <- uniqLabel "cg"
  lbl2 <- uniqLabel "cg"
  return $ "cmp " ++ parms ++ "\n"
    ++ jt ++ " " ++ lbl1 ++ "\n"
    ++ "mov " ++ d ++ ", dword 0\n"
    ++ "jmp " ++ lbl2 ++ "\n"
    ++ lbl1 ++ ":\n"
    ++ "mov " ++ d ++ ", dword 1\n"
    ++ lbl2 ++ ":\n"


cgOP :: LLop -> String
cgOP OPcp  = "mov"
cgOP OPadd = "add"
cgOP OPsub = "sub"
cgOP OPmul = "imul"
cgOP OPdiv = error "Division needs special care on i386. Do not use cgOP."
cgOP OPmod = error "Modulo needs special care on i386. Do not use cgOP."
cgOP OPand = "and"
cgOP OPor  = "or"
cgOP OPxor = "xor"
cgOP OPdec = "dec"
cgOP OPinc = "inc"
cgOP OPnot = "not"
cgOP OPneg = "neg"
cgOP OPret = "ret"
cgOP OPspsub = "sub esp,"
cgOP OPspadd = "add esp,"
cgOP OPpush  = "push"
cgOP OPpop   = "pop"
cgOP OPsrcline = []
cgOP OPcall  = "call"
cgOP OPlabel = []
cgOP OPjmp   = "jmp"
cgOP OPjmpz  = []
cgOP OPjmpnz = []

cgJumpCMP :: LLcmp -> String
cgJumpCMP CMPlt = "jl"
cgJumpCMP CMPgt = "jg"
cgJumpCMP CMPle = "jle"
cgJumpCMP CMPge = "jge"
cgJumpCMP CMPeq = "je"
cgJumpCMP CMPneq = "jne"
cgJumpCMP CMPand = []
cgJumpCMP CMPor = []



cgLine :: LLcmd -> SIM String
-- Some virtual statements for control purposes.
cgLine (LLdecl v (MaliceArraySize _ _)) =
  cgLine (LLcmd OPcp (Two (Pvar v) (Pimm 0)))
cgLine (LLdecl v (MaliceArray _)) =
  cgLine (LLcmd OPcp (Two (Pvar v) (Pimm 0)))
cgLine (LLdecl v _) = do
--cgLine (LLDecl v t) = do
  --sts <- getSymTabs
  --st <- popSymTab
  --pushSymTab $ M.insert v (t, sum $ map M.size sts) st
  initzero <- cgLine (LLcmd OPcp (Two (Pvar v) (Pimm 0)))
  return initzero
cgLine (LLcmd OPlabel (One (Plbl l)))
  = return $ l ++ ":\n"
cgLine (LLcmd OPsrcline (One (Pimm i)))
  = return $ "\n; Source line " ++ show i ++ "\n"
cgLine (LLcmd OPret Zero) = do
  fn <- getFuncName
  return $ "jmp end_" ++ fn ++ "\n"
cgLine (LLscope symtab ll) = do
  pushSymTab symtab
  c <- cgLL ll
  _ <- popSymTab
  return c
--
-- Division and modulo. Special care on i386!
cgLine (LLcmd OPdiv (Two (Preg r1) (Preg r2)))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = return $
    "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ registerName r1 ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ registerName r2 ++ "\n"
    ++ "mov " ++ registerName r1 ++ ", eax\n"
    ++ (if r1 /= 3 then "pop edx\n" else "add esp, 4\n")
    ++ (if r1 /= 0 then "pop eax\n" else "add esp, 4\n")
cgLine (LLcmd OPmod (Two (Preg r1) (Preg r2)))
-- WARNING: If r1 == 3 and r2 == 0 we will have loss of information...
-- To be fixed later with register allocation.
  = return $ "push eax\n"
    ++ "push edx\n"
    ++ "mov eax, " ++ registerName r1 ++ "\n"
    ++ "mov edx, 0\n"
    ++ "idiv " ++ registerName r2 ++ "\n"
    ++ "mov " ++ registerName r1 ++ ", edx\n"
    ++ (if r1 /= 3 then "pop edx\n" else "add esp, 4\n")
    ++ (if r1 /= 0 then "pop eax\n" else "add esp, 4\n")
cgLine (LLcmd OPdiv (Two (Preg r1) (Pimm imm))) =
  cgLL [ LLcmd OPcp (Two (Preg $ r1+1) (Pimm imm))
       , LLcmd OPdiv (Two (Preg r1) (Preg $ r1+1))
       ]
cgLine (LLcmd OPmod (Two (Preg r1) (Pimm imm))) =
  cgLL [ LLcmd OPcp (Two (Preg $ r1+1) (Pimm imm))
       , LLcmd OPmod (Two (Preg r1) (Preg $ r1+1))
       ]
cgLine (LLcmd OPdiv _)
  = error "cgLine: Impossible operand combination for LLDiv on i386"
cgLine (LLcmd OPmod _)
  = error "cgLine: Impossible operand combination for LLMod on i386"
--
-- Conditional jumps.
cgLine (LLcmd OPjmpz (Two (Plbl l) p1)) = do
  parms <- cgLL2param p1 p1
  return $
    "test " ++ parms ++ "\n"
    ++ "je " ++ l ++ "\n"
cgLine (LLcmd OPjmpnz (Two (Plbl l) p1)) = do
  parms <- cgLL2param p1 p1
  return $
    "test " ++ parms ++ "\n"
    ++ "jne " ++ l ++ "\n"
--
-- Comparisons.
cgLine (LLcmp CMPand ps) = cgLine (LLcmd OPand ps)
cgLine (LLcmp CMPor  ps) = cgLine (LLcmd OPor  ps)
cgLine (LLcmp cmp (Two p1 p2)) =
  cgCmp (cgJumpCMP cmp) p1 p1 p2
cgLine (LLcmp _ _) = error "CodeGen: Invalid parameters for comparison."
--
-- Commands.
cgLine (LLcmd op ps) = do
  parms <- cgLLparams ps
  return $ cgOP op ++ " " ++ parms ++ "\n"
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
              ++ "extern _malice_alloc\n"
              ++ "extern _check_arr\n"
              ++ "extern _print_char\n"
              ++ "extern _print_string\n"
              ++ "extern _print_int\n"
              ++ "extern _read_int\n"




cgFunc :: [LLcmd] -> SIM String
--cgDA (Function symtab name arglist rettype body)
cgFunc body = do
  fn <- getFuncName
  varcount <- getMaxVarCtr
  cBody <- cgLL body
  cSave <- cgLL [LLcmd OPpush (One $ Preg 1), LLcmd OPpush (One $ Preg 2)]
  cAlloc <- cgLine (LLcmd OPspsub $ One $ Pimm $ fromIntegral (4 * varcount))
  cDealloc  <- cgLine (LLcmd OPspadd $ One $ Pimm $ fromIntegral (4 * varcount))
  cRestore <- cgLL [LLcmd OPpop (One $ Preg 2), LLcmd OPpop (One $ Preg 1)]
  return $ "\n"
    ++ "global " ++ fn ++ "\n"
    ++ fn ++ ":\n\n"
    ++ cSave
    ++ "push ebp\n"
    ++ "mov ebp, esp\n"
    ++ cAlloc
    ++ "\n"
    ++ cBody
    ++ "mov eax, 0\n"
    ++ "\n"
    ++ "end_" ++ fn ++ ":\n"
    ++ cDealloc
    ++ "pop ebp\n" 
    ++ cRestore
    ++ "ret\n"


cgDA :: DeclarationAct -> StringTable -> (String, StringTable)
cgDA (Function symtab name arglist _ body) stt = (asmcode, newstt)
  where
    asmcode = evalState (cgFunc llcode) (name, fargssyt, [newsyt], newstt, (-1, -1), 0, mvc)
    (llcode, newsyt, newstt, mvc) = llFunc body name fargssyt symtab stt
    fargssyt = funcArgsSymTab arglist



cgD :: Declaration -> StringTable -> (String, StringTable)
--cgD (pos, (Function symtab name arglist rettype body))
cgD ( _, da ) = cgDA da



cgDL :: DeclarationList -> StringTable -> (String, StringTable)
cgDL [] stt
  = ("", stt)
cgDL (d : ds) stt
  = (code ++ code2, newstt2)
  where
    (code2, newstt2) = cgDL ds newstt
    (code, newstt) = cgD d stt


cgAST :: AST -> String
cgAST (AST _ dl)
  = asmPrologue
    ++ code
    ++ "\n\n"
    ++ "section .data\n"
    ++ concat ["_str_" ++ show i ++ ": db " ++ strToAsm s ++ ",0\n" | (i, s) <- strtab]
  where
    (code, strtab) = cgDL dl []
