module LLGen
       (
         LLcmd (..), LLparam (..), LLparams (..), LLop(..), LLcmp(..),
         llFunc
       ) where

import Common
import CGCommon
import OptimiseExpr
import Data.Bits
import Data.Char
import Control.Monad.State
--import qualified Data.Map as M


--type VarOffset = Int
--data VarValue = Imm Immediate
--data VarInfo = SVar VarOffset VarValue


data LLparam = Pvar Variable
             | Preg Register
             | Pimm Immediate
             | Plbl Label
             | Pderef LLparam LLparam
             deriving (Show, Eq)


data LLcmd = LLcmd LLop LLparams
           | LLcmp LLcmp LLparams
           | LLscope SymbolTable [LLcmd]
           | LLdecl Variable MaliceType

data LLparams = Zero | One LLparam | Two LLparam LLparam

data LLop = OPcp
          | OPadd
          | OPsub
          | OPmul
          | OPdiv
          | OPmod
          | OPand
          | OPor
          | OPxor
          | OPdec
          | OPinc
          | OPnot
          | OPneg
          | OPret
          | OPspsub
          | OPspadd
          | OPpush
          | OPpop
          | OPsrcline
          | OPcall
          | OPlabel
          | OPjmp
          | OPjmpz
          | OPjmpnz
          deriving (Show, Eq)

data LLcmp = CMPlt
           | CMPgt
           | CMPle
           | CMPge
           | CMPeq
           | CMPneq
           | CMPand
           | CMPor


llBinOp :: Operand -> LLparams -> LLcmd
llBinOp "+" = LLcmd OPadd
llBinOp "-" = LLcmd OPsub
llBinOp "*" = LLcmd OPmul
llBinOp "/" = LLcmd OPdiv
llBinOp "%" = LLcmd OPmod
llBinOp "&" = LLcmd OPand
llBinOp "|" = LLcmd OPor
llBinOp "^" = LLcmd OPxor
llBinOp "<" = LLcmp CMPlt
llBinOp ">" = LLcmp CMPgt
llBinOp "<=" = LLcmp CMPle
llBinOp ">=" = LLcmp CMPge
llBinOp "==" = LLcmp CMPeq
llBinOp "!=" = LLcmp CMPneq
llBinOp "&&" = LLcmp CMPand
llBinOp "||" = LLcmp CMPor
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> LLparams -> LLcmd
llUnOp "~" = LLcmd OPnot
llUnOp "-" = LLcmd OPneg
llUnOp op = error $ "llUnOp: Invalid operand encountered: " ++ op

truncates32tou8 :: Immediate -> Immediate
truncates32tou8 i = 255 .&. i



-- The following code does a simple stack machine for evaluating Exprs.
llExp :: Expr -> SIM [LLcmd]
llExp (BinOp op exp1 (Int imm)) = do
  e1 <- llExp exp1
  return (e1 ++ [(llBinOp op) (Two (Preg 0) (Pimm imm))])
llExp (BinOp op exp1 exp2) = do
  e1 <- llExp exp1
  e2 <- llExp exp2
  lblcnt <- uniqLabel "ll"
  lblend <- uniqLabel "ll"
  return $ e1
    ++ [LLcmd OPpush (One $ Preg 0)]
    ++ (if (op == "&&")
        then [ LLcmd OPjmpnz (Two (Plbl lblcnt) (Preg 0))
             , LLcmd OPspadd (One $ Pimm 4)
             , LLcmd OPcp (Two (Preg 0) (Pimm 0))
             , LLcmd OPjmp (One $ Plbl lblend)
             , LLcmd OPlabel (One $ Plbl lblcnt)
             ]
        else []
       )
    ++ (if op == "||"
        then [ LLcmd OPjmpz (Two (Plbl lblcnt) (Preg 0))
             , LLcmd OPspadd (One $ Pimm 4)
             , LLcmd OPcp (Two (Preg 0) (Pimm 1))
             , LLcmd OPjmp (One $ Plbl lblend)
             , LLcmd OPlabel (One $ Plbl lblcnt)
             ]
        else []
       )
    ++ e2
    ++ [LLcmd OPcp (Two (Preg 1) (Preg 0))]
    ++ [LLcmd OPpop (One $ Preg 0)]
    ++ [(llBinOp op) (Two (Preg 0) (Preg 1))]
    ++ (if (op == "&&" || op == "||")
        then [LLcmd OPlabel (One $ Plbl lblend)]
        else []
       )
llExp (UnOp op (Int imm))
  = return [LLcmd OPcp (Two (Preg 0) (Pimm (evalUnOp op imm)))]
llExp (UnOp op exp1) = do
  e1 <- llExp exp1
  return $ e1
           ++ [(llUnOp op) (One $ Preg 0)]
llExp (Int i)
  = return [LLcmd OPcp (Two (Preg 0) (Pimm i))]
llExp (Char c)
  = return [LLcmd OPcp (Two (Preg 0) (Pimm (truncates32tou8 $ fromIntegral (ord c) :: Immediate)))]
llExp (Id (SingleElement var))
  = return [LLcmd OPcp (Two (Preg 0) (Pvar var))]
llExp (FunctionCall fn args) = do
  llargs <- mapM llExp args
  return $ ( concat $ reverse $ map (flip (++) [LLcmd OPpush (One $ Preg 0)]) $ llargs )
           ++ [ LLcmd OPcall (One $ Plbl fn)
              , LLcmd OPspadd (One $ Pimm $ fromIntegral (4 * length args))
              ]
llExp (String str) = do
  strlbl <- uniqStr str
  return [LLcmd OPcp (Two (Preg 0) (Plbl strlbl))]
llExp (Id (ArrayElement v nexp)) = do
  llnexp <- llExp nexp
  checkcall <- llExp (FunctionCall "_check_arr" [])
  (line, _) <- getCodePos
  return $ llnexp
           ++ [ LLcmd OPpush (One $ Preg 0)
              , LLcmd OPpush (One $ Pvar v)
              , LLcmd OPpush (One $ Pimm $ fromIntegral line)
              ]
           ++ checkcall  -- if array access is invalid this never returns
           ++ [LLcmd OPspadd (One $ Pimm 12)]
           ++ [ LLcmd OPcp (Two (Preg 1) (Preg 0))
              , LLcmd OPcp (Two (Preg 0) (Pvar v))
              , LLcmd OPcp (Two (Preg 0) (Pderef (Preg 0) (Preg 1)))
              ]



llSA :: StatementAct -> SIM [LLcmd]
llSA (Declare t@(MaliceArraySize _ sexp) v) = do
  llsexp <- llExp sexp
  malloccall <- llExp $ FunctionCall "_malice_alloc" []
  (line, _) <- getCodePos
  return $ [LLdecl v t]
           ++ llsexp
           ++ [LLcmd OPpush (One $ Preg 0)] -- the logical size
           ++ [LLcmd OPadd (Two (Preg 0) (Pimm 1))] -- + the hidden size field
           ++ [LLcmd OPmul (Two (Preg 0) (Pimm 4))]
           ++ [LLcmd OPpush (One $ Pimm $ fromIntegral line)]
           ++ [LLcmd OPpush (One $ Preg 0)] -- the physical size
           ++ malloccall
           ++ [LLcmd OPspadd (One $ Pimm 8)] -- discard physical size
           ++ [LLcmd OPcp (Two (Pvar v) (Preg 0))]
           ++ [LLcmd OPpop (One $ Preg 1)] -- the logical size + hidden field
           ++ [LLcmd OPcp (Two (Pderef (Preg 0) (Pimm 0)) (Preg 1))]
llSA (Declare t v)
  = return [LLdecl v t]
llSA (Assign (SingleElement var) (Int imm))
  = return [LLcmd OPcp (Two (Pvar var) (Pimm imm))]
llSA (Assign (SingleElement var) exp1) = do
  e1 <- llExp (optimiseExpr exp1)
  return $ e1
           ++ [(LLcmd OPcp (Two (Pvar var) (Preg 0)))]
llSA (Assign (ArrayElement var nexp) exp1) = do
  llnexp <- llExp nexp
  e1 <- llExp (optimiseExpr exp1)
  checkcall <- llExp (FunctionCall "_check_arr" [])
  (line, _) <- getCodePos
  return $ llnexp
           ++ [ LLcmd OPpush (One $ Preg 0)
              , LLcmd OPpush (One $ Pvar var)
              , LLcmd OPpush (One $ Pimm $ fromIntegral line)
              ]
           ++ checkcall  -- if array access is invalid this never returns
           ++ [LLcmd OPspadd (One $ Pimm 8)]
           ++ e1
           ++ [LLcmd OPcp (Two (Preg 2) (Pvar var))]
           ++ [LLcmd OPpop (One $ Preg 1)]
           ++ [LLcmd OPcp (Two (Pderef (Preg 2) (Preg 1)) (Preg 0))]
llSA (Decrease (SingleElement var))
  = return [(LLcmd OPdec (One $ Pvar var))]
llSA (Decrease (ArrayElement var nexp)) = do
  llnexp <- llExp nexp
  checkcall <- llExp (FunctionCall "_check_arr" [])
  (line, _) <- getCodePos
  return $ llnexp
           ++ [ LLcmd OPpush (One $ Preg 0)
              , LLcmd OPpush (One $ Pvar var)
              , LLcmd OPpush (One $ Pimm $ fromIntegral line)
              ]
           ++ checkcall  -- if array access is invalid this never returns
           ++ [LLcmd OPspadd (One $ Pimm 12)]
           ++ [LLcmd OPcp (Two (Preg 1) (Pvar var))]
           ++ [(LLcmd OPdec (One $ Pderef (Preg 1) (Preg 0)))]
llSA (Increase (SingleElement var))
  = return [(LLcmd OPinc (One $ Pvar var))]
llSA (Increase (ArrayElement var nexp)) = do
  llnexp <- llExp nexp
  checkcall <- llExp (FunctionCall "_check_arr" [])
  (line, _) <- getCodePos
  return $ llnexp
           ++ [ LLcmd OPpush (One $ Preg 0)
              , LLcmd OPpush (One $ Pvar var)
              , LLcmd OPpush (One $ Pimm $ fromIntegral line)
              ]
           ++ checkcall  -- if array access is invalid this never returns
           ++ [LLcmd OPspadd (One $ Pimm 12)]
           ++ [LLcmd OPcp (Two (Preg 1) (Pvar var))]
           ++ [(LLcmd OPinc (One $ Pderef (Preg 1) (Preg 0)))]
llSA (Return exp1) = do
  e1 <- llExp (optimiseExpr exp1)
  return $ e1
           ++ [LLcmd OPret Zero]
llSA (Print (String str)) = do
  fc <- (llExp (FunctionCall "_print_string" [String str]))
  return fc
llSA (Print (Char c)) = do
  fc <- (llExp (FunctionCall "_print_char" [Char c]))
  return fc
llSA (Print (Id (SingleElement v))) = do
  Just (vt, _) <- lookupSym v
  fc <- llExp (FunctionCall (case vt of
                               MaliceChar -> "_print_char"
                               MaliceString -> "_print_string"
                               MaliceInt -> "_print_int"
                               _ -> error $ "Cannot print a " ++ (show vt)
                            )
                            []
              )
  return $ [LLcmd OPpush (One $ Pvar v)]
           ++ fc
           ++ [LLcmd OPspadd (One $ Pimm 4)]
llSA (Print exp1) = do
  e1 <- (llExp (optimiseExpr exp1))
  fc <- (llExp (FunctionCall "_print_int" []))
  return $ e1 ++ [LLcmd OPpush (One $ Preg 0)] ++ fc ++ [LLcmd OPspadd (One $ Pimm 4)]
llSA (Get (SingleElement var)) = do
  e1 <- llExp (FunctionCall "_read_int" [])
  return $ e1 ++ [LLcmd OPcp (Two (Pvar var) (Preg 0))]
llSA (Get (ArrayElement var nexp)) = do
  llnexp <- llExp nexp
  e1 <- llExp (FunctionCall "_read_int" [])
  checkcall <- llExp (FunctionCall "_check_arr" [])
  (line, _) <- getCodePos
  return $ llnexp
           ++ [ LLcmd OPpush (One $ Preg 0)
              , LLcmd OPpush (One $ Pvar var)
              , LLcmd OPpush (One $ Pimm $ fromIntegral line)
              ]
           ++ checkcall  -- if array access is invalid this never returns
           ++ [LLcmd OPspadd (One $ Pimm 8)]
           ++ e1
           ++ [LLcmd OPcp (Two (Preg 2) (Pvar var))]
           ++ [LLcmd OPpop (One $ Preg 1)]
           ++ [LLcmd OPcp (Two (Pderef (Preg 2) (Preg 1)) (Preg 0))]
llSA (FunctionCallS fc@(FunctionCall _ _))
  = llExp fc
llSA (FunctionCallS _) = do
  epos <- showCodePos
  error $ epos ++ "Cannot call an expression that is not a FunctionCall"
llSA (Until syt e body) = do
  startlbl <- uniqLabel "ll"
  endlbl <- uniqLabel "ll"
  le <- llExp e
  pushSymTab syt
  lb <- llSL body
  newsyt <- popSymTab
  return $ [LLcmd OPlabel (One $ Plbl startlbl)]
           ++ le
           ++ [ LLcmd OPjmpnz (Two (Plbl endlbl) (Preg 0))
              , LLscope newsyt lb
              , LLcmd OPjmp (One $ Plbl startlbl)
              , LLcmd OPlabel (One $ Plbl endlbl)
              ]
llSA (IfElse list) = do
  endlbl <- uniqLabel "ll"
  ls <- mapM (\(syt, e, body) -> do
                                 le <- llExp e
                                 pushSymTab syt
                                 lb <- llSL body
                                 newsyt <- popSymTab
                                 lbl <- uniqLabel "ll"
                                 return $ le
                                          ++ [ LLcmd OPjmpz (Two (Plbl lbl) (Preg 0))
                                             , LLscope newsyt lb
                                             , LLcmd OPjmp (One $ Plbl endlbl)
                                             , LLcmd OPlabel (One $ Plbl lbl)
                                             ]
             ) list
  return $ concat $ (ls ++ [[LLcmd OPlabel (One $ Plbl endlbl)]])



llS :: Statement -> SIM [LLcmd]
llS s = do
  putCodePos cp
  ll <- llSA sa
  return $ (LLcmd OPsrcline (One $ Pimm $ fromIntegral line)) : ll
  where
    (cp@(line, _), sa) = s


llSL :: StatementList -> SIM [LLcmd]
llSL ss = do
  localsyt <- popSymTab
  localsyt2 <- scanSymTab localsyt
  pushSymTab $ localsyt2
  l <- mapM llS ss
  return $ concat l
--  si <- get
--  return $ concat $ map (((flip evalState) si).llS) ss


--        Code body          /--/          Func args      Scope local
llFunc :: StatementList -> FunctionName -> SymbolTable -> SymbolTable -> StringTable -> ([LLcmd], SymbolTable, StringTable, MaxVarCount)
llFunc sl fn fargs syt stt
  = (code, newsyt, strtab, mvc)
  where
    (code, (_, _, [newsyt], strtab, _, _, mvc))
      = (((flip runState) (fn, fargs, [syt], stt, (-1, -1), 0, 0)).llSL) sl

