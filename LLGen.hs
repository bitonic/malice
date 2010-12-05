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

data LLParam = PVar Variable
             | PReg Register
             | PImm Immediate
             | PLbl Label
             | PStr String
             deriving (Show, Eq)


data LLcmd
--Copy Dest Src
     = LLCp LLParam LLParam
--Set Dest value
     | LLAdd LLParam LLParam
     | LLSub LLParam LLParam
     | LLMul LLParam LLParam
     | LLDiv LLParam LLParam
     | LLMod LLParam LLParam
     | LLAnd LLParam LLParam
     | LLOr LLParam LLParam
     | LLXOr LLParam LLParam
     | LLDec LLParam
     | LLInc LLParam
     | LLNot LLParam
     | LLClt LLParam LLParam
     | LLCgt LLParam LLParam
     | LLCle LLParam LLParam
     | LLCge LLParam LLParam
     | LLCeq LLParam LLParam
     | LLCneq LLParam LLParam
     | LLCand LLParam LLParam
     | LLCor LLParam LLParam
--Return: Have the return value ready in register 0!
     | LLRet
     | LLSpSub Immediate
     | LLSpAdd Immediate
--     | LLPush Register
--     | LLPushImm Immediate
--     | LLPop Register
     | LLPush LLParam
     | LLPop LLParam
     | LLSrcLine Immediate
     | LLCall String
     | LLLabel Label
     | LLScope SymbolTable [LLcmd]
     | LLJmp Label
     | LLJmpZ Label LLParam
     | LLJmpNZ Label LLParam
     deriving (Show, Eq)






llBinOp :: Operand -> LLParam -> LLParam -> LLcmd
llBinOp "+" = LLAdd
llBinOp "-" = LLSub
llBinOp "*" = LLMul
llBinOp "/" = LLDiv
llBinOp "%" = LLMod
llBinOp "&" = LLAnd
llBinOp "|" = LLOr
llBinOp "^" = LLXOr
llBinOp "<" = LLClt
llBinOp ">" = LLCgt
llBinOp "<=" = LLCle
llBinOp ">=" = LLCge
llBinOp "==" = LLCeq
llBinOp "!=" = LLCneq
llBinOp "&&" = LLCand
llBinOp "||" = LLCor
llBinOp op = error ("llBinOp: Invalid operand encountered: " ++ op)

llUnOp :: Operand -> LLParam -> LLcmd
llUnOp "~" = LLNot
llUnOp "-" = error "Implement EXP -"
llUnOp op = error $ "llUnOp: Invalid operand encountered: " ++ op

truncates32tou8 :: Immediate -> Immediate
truncates32tou8 i = 255 .&. i
{-
  = if (i < 0)
    then (256 - (i .&. 255))
    else (i .&. 255)
-}




-- The following code does a simple stack machine for evaluating Exprs.
llExp :: Expr -> Register -> SIM [LLcmd]
llExp (BinOp op exp1 (Int imm)) destreg = do
  e1 <- llExp exp1 destreg
  return (e1 ++ [llBinOp op (PReg destreg) (PImm imm)])
llExp (BinOp op exp1 exp2) destreg = do
  e1 <- (llExp exp1 destreg)
--  e2 <- (llExp exp2 (succ destreg))
  e2 <- (llExp exp2 destreg)
  return $ e1
    ++ [LLPush (PReg destreg)]
    ++ e2
    ++ [LLCp (PReg (succ destreg)) (PReg destreg)]
    ++ [LLPop (PReg destreg)]
    ++ [llBinOp op (PReg destreg) (PReg (succ destreg))]
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
    ++ ( concat $ map (flip (++) [LLPush (PReg destreg)]) $ llargs )
    ++ [LLCall fn, LLSpAdd $ fromIntegral (4 * length args)]
    ++ (if destreg == 0 then [] else [LLCp (PReg destreg) (PReg 0), LLPop (PReg 0)])
llExp (String str) destreg = do
  strlbl <- uniqStr str
  return $ [LLCp (PReg destreg) (PLbl strlbl)]
llExp (Id (ArrayElement _ _)) _
  = error "Implement EXP ArrayElement"



llSA :: StatementAct -> SIM [LLcmd]
llSA (Declare _ _)
  = return $ []
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
llSA (Print exp1) = do
  e1 <- (llExp (optimiseExpr exp1) 0)
  fc <- (llExp (FunctionCall "_print_int" []) 0)
  return $ e1 ++ [LLPush (PReg 0)] ++ fc ++ [LLSpAdd 4]
llSA (Get (SingleElement var)) = do
  e1 <- (llExp (FunctionCall "_readint" []) 0)
  return $ e1 ++ [LLCp (PVar var) (PReg 0)]
llSA (Get (ArrayElement _ _)) = do
  epos <- showCodePos
  error $ epos ++ "Cannot read a whole array"
llSA (FunctionCallS fc@(FunctionCall _ _)) = do
  e <- llExp fc 0
  return e
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

