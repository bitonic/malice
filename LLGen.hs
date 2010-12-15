module LLGen
       (
         LLcmd (..), LLParam (..),
         llSL,
       ) where

import Common
import CGCommon
import OptimExpr
import Data.Bits
import Data.Char
--import Control.Monad.State


--type VarOffset = Int
--data VarValue = Imm Immediate
--data VarInfo = SVar VarOffset VarValue

data LLparam = Pvar Variable
             | Preg Register
             | Pimm Immediate
             | Plbl Label
--           | PStr String
             deriving (Show, Eq)


data LLcmd = LLcmd LLop LLparams
           | LLscope SymbolTable [LLcmd]

data LLparams = One LLparam | Two LLparam LLparam | Zero

data LLop = OPadd
          | OPdecl
          | OPcp
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
          | OPclt
          | OPcgt
          | OPcle
          | OPcge
          | OPceq
          | OPcneq
          | OPcand
          | OPcor
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
            


llBinOp :: Operand -> LLparam -> LLparam -> LLcmd
llBinOp "+" = LLcmd . OPadd . Two
llBinOp "-" = LLcmd . OPsub . Two
llBinOp "*" = LLcmd . OPmul . Two
llBinOp "/" = LLcmd . OPdiv . Two
llBinOp "%" = LLcmd . OPmod . Two
llBinOp "&" = LLcmd . OPand . Two
llBinOp "|" = LLcmd . OPor . Two
llBinOp "^" = LLcmd . OPxor . Two
llBinOp "<" = LLcmd . OPclt . Two
llBinOp ">" = LLcmd . OPcgt . Two
llBinOp "<=" = LLcmd . OPcle . Two
llBinOp ">=" = LLcmd . OPcge . Two
llBinOp "==" = LLcmd . OPceq . Two
llBinOp "!=" = LLcmd . OPcneq . Two
llBinOp "&&" = LLcmd . OPcand . Two
llBinOp "||" = LLcmd . OPcor . Two
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> LLparam -> LLcmd
llUnOp "~" = LLcmd . OPnot . One
llUnOp "-" = LLcmd . OPneg . One
llUnOp op = error $ "llUnOp: Invalid operand encountered: " ++ op

truncates32tou8 :: Immediate -> Immediate
truncates32tou8 i = 255 .&. i


-- The following code does a simple stack machine for evaluating Exprs.
llExp :: Expr -> Register -> SIM [LLcmd]
llExp (BinOp op exp1 (Int imm)) destreg = do
  e1 <- llExp exp1 destreg
  return (e1 ++ [llBinOp op (Preg destreg) (Pimm imm)])
llExp (BinOp op exp1 exp2) destreg = do
  e1 <- (llExp exp1 destreg)
--  e2 <- (llExp exp2 (succ destreg))
  e2 <- (llExp exp2 destreg)
  lblcnt <- uniqLabel
  lblend <- uniqLabel
  return $ e1
    ++ [LLpush (Preg destreg)]
    ++ (if (op == "&&")
        then [ LLcmd OPjmpnz (Two (Plbl lblcnt) (Preg destreg))
             , LLcmd OPspadd (One $ Pimm 4)
             , LLcmd OPcp (Two (Preg destreg) (Pimm 0))
             , LLcmd OPjmp (One $ Plbl lblend)
             , LLcmd OPlabel (One $ Plbl lblcnt)
             ]
        else [])
    ++ (if op == "||"
        then [ LLcmd OPjmpz (Two (Plbl lblcnt) (Preg destreg))
             , LLcmd OPspadd (One $ Pimm 4)
             , LLcmd OPcp (Two (Preg destreg) (PImm 1)),
             , LLcmd OPjmp (One $ Plbl lblend)
             , LLcmd OPlabel (One $ Plbl lblcnt)
             ]
        else [])
    ++ e2
    ++ [LLCp (PReg (succ destreg)) (PReg destreg)]
    ++ [LLPop (PReg destreg)]
    ++ [llBinOp op (PReg destreg) (PReg (succ destreg))]
    ++ (if (op == "&&" || op == "||")
        then [LLLabel lblend]
        else [])
llExp (UnOp op (Int imm)) destreg
  = return $ [LLCp (PReg destreg) (PImm (evalUnOp op imm))]
llExp (UnOp op exp1) destreg = do
  e1 <- (llExp exp1 destreg)
  return $ e1
           ++ [llUnOp op (PReg destreg)]
llExp (Int i) destreg
  = return $ [LLCp (PReg destreg) (PImm i)]
llExp (Char c) destreg
  = return $ [LLCp (PReg destreg) (PImm (truncates32tou8 $ fromIntegral (ord c) :: Immediate))]
llExp (Id (SingleElement var)) destreg
  = return $ [LLCp (PReg destreg) (PVar var)]
llExp (FunctionCall fn args) destreg = do
  llargs <- mapM (flip (llExp) destreg) args
  return $ (if destreg == 0 then [] else [LLPush (PReg 0)])
    ++ ( concat $ reverse $ map (flip (++) [LLPush (PReg destreg)]) $ llargs )
    ++ [LLCall fn, LLSpAdd $ fromIntegral (4 * length args)]
    ++ (if destreg == 0 then [] else [LLCp (PReg destreg) (PReg 0), LLPop (PReg 0)])
llExp (String str) destreg = do
  strlbl <- uniqStr str
  return $ [LLCp (PReg destreg) (PLbl strlbl)]
llExp (Id (ArrayElement _ _)) _
  = error "Implement EXP ArrayElement"



llSA :: StatementAct -> SIM [LLcmd]
llSA (Declare t v)
  = return $ [LLDecl v t]
llSA (Assign (SingleElement var) (Int imm))
  = return $ [LLCp (PVar var) (PImm imm)]
llSA (Assign (SingleElement var) exp1) = do
  e1 <- (llExp (optimiseExpr exp1) 0)
  return $ e1 ++ [(LLCp (PVar var) (PReg 0))]
llSA (Assign (ArrayElement _ _) _)
  = error "Implement Assign ArrayElement"
llSA (Decrease (SingleElement var))
  = return $ [(LLCp (PReg 0) (PVar var)), (LLDec (PReg 0)), (LLCp (PVar var) (PReg 0))]
llSA (Decrease (ArrayElement _ _))
  = error "Implement Decrease ArrayElement"
llSA (Increase (SingleElement var))
  = return $ [(LLCp (PReg 0) (PVar var)), (LLInc (PReg 0)), (LLCp (PVar var) (PReg 0))]
llSA (Increase (ArrayElement _ _))
  = error "Implement Increase ArrayElement"
llSA (Return exp1) = do
  e1 <- (llExp (optimiseExpr exp1) 0)
  return $ e1 ++  [LLRet]
llSA (Print (String str)) = do
  fc <- (llExp (FunctionCall "_print_string" [String str]) 0)
  return fc
llSA (Print (Char c)) = do
  fc <- (llExp (FunctionCall "_print_char" [Char c]) 0)
  return fc
llSA (Print (Id (SingleElement v))) = do
  Just (vt, _) <- lookupSym v
  fc <- (llExp (FunctionCall (case vt of
                              MaliceChar -> "_print_char"
                              MaliceString -> "_print_string"
                              MaliceInt -> "_print_int"
                              _ -> error $ "Cannot print a " ++ (show vt)
                             ) []) 0)
  return $ [LLPush (PVar v)] ++ fc ++ [LLSpAdd 4]
llSA (Print exp1) = do
  e1 <- (llExp (optimiseExpr exp1) 0)
  fc <- (llExp (FunctionCall "_print_int" []) 0)
  return $ e1 ++ [LLPush (PReg 0)] ++ fc ++ [LLSpAdd 4]
llSA (Get (SingleElement var)) = do
  e1 <- (llExp (FunctionCall "_read_int" []) 0)
  return $ e1 ++ [LLCp (PVar var) (PReg 0)]
llSA (Get (ArrayElement _ _)) = do
  epos <- showCodePos
  error $ epos ++ "Cannot read a whole array"
llSA (FunctionCallS fc@(FunctionCall _ _))
  = llExp fc 0
llSA (FunctionCallS _) = do
  epos <- showCodePos
  error $ epos ++ "Cannot call an expression that is not a FunctionCall"
llSA (Until syt e body) = do
  startlbl <- uniqLabel
  endlbl <- uniqLabel
  le <- llExp e 0
  lb <- llSL body
  return $ [LLLabel startlbl] ++ le ++ [LLJmpNZ endlbl (PReg 0),
                                        LLScope syt lb,
                                        LLJmp startlbl,
                                        LLLabel endlbl]
llSA (IfElse list) = do
  endlbl <- uniqLabel
  ls <- mapM (\(syt, e, body) -> do
                                 le <- llExp e 0
                                 lb <- llSL body
                                 lbl <- uniqLabel
                                 return $ le ++ [LLJmpZ lbl (PReg 0), LLScope syt lb, LLJmp endlbl, LLLabel lbl]
             ) list
  return $ concat $ (ls ++ [[LLLabel endlbl]])



llS :: Statement -> SIM [LLcmd]
llS s = do
  ll <- llSA sa
  putCodePos cp
  return $ (LLSrcLine $ fromIntegral line) : ll
  where
    (cp@(line, _), sa) = s

llSL :: StatementList -> SIM [LLcmd]
llSL ss = do
  l <- mapM llS ss
  return $ concat l
--  si <- get
--  return $ concat $ map (((flip evalState) si).llS) ss

