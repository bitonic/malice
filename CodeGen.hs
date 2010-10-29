module CodeGen where

import List
import Parser



-- Always from right to left, Intel-style

data LLcmd
--Copy Dest Src
     = LLCpRegVar Int String
	 | LLCpVarReg String Int
	 | LLCpRegImm Int Int
--Set Dest value
	 | LLAdd Int Int
	 | LLMul Int Int
	 | LLDec Int
	 | LLInc Int
--Return: Have the return value ready in register 0!
	 | LLRet
     deriving (Show, Eq)



llProgram :: Program -> [LLcmd]
llProgram (Program statlist)
  = (llStatlist statlist (0, 4))

llStatlist :: StatementList -> (Int, Int) -> [LLcmd]
llStatlist ((Declare var) : ss) (destreg, maxreg)
  = llStatlist ss (destreg, maxreg)
llStatlist ((Assign var exp) : ss) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [(LLCpVarReg var destreg)]
	++ (llStatlist ss (destreg, maxreg))
llStatlist ((Decrease var) : ss) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLDec destreg), (LLCpVarReg var destreg)]
llStatlist ((Increase var) : ss) (destreg, maxreg)
  = [(LLCpRegVar destreg var), (LLInc destreg), (LLCpVarReg var destreg)]
llStatlist ((Return exp) : ss) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [LLRet]
llStatlist [] _
  = []


llExp :: Exp -> (Int, Int) -> [LLcmd]
llExp (BinOp op exp1 exp2) (destreg, maxreg)
  = (llExp exp1 (destreg, maxreg))
	++ (llExp exp2 ((destreg + 1), maxreg))
	++ [llBinOp op destreg (destreg + 1)]
--llExp (UnOp "~" exp)
--  = [Set "ans" 42]
llExp (Int i) (destreg, maxreg)
  = [LLCpRegImm destreg i]
llExp (Var var) (destreg, maxreg)
  = [LLCpRegVar destreg var]


llBinOp :: String -> Int -> Int -> LLcmd
llBinOp "+" i j
  = LLAdd i j
llBinOp "*" i j
  = LLMul i j



-- The following is the MAlice2C converter. /max

convertProgramToC :: Program -> String
convertProgramToC (Program statlist)
  = "int main()\n{\n"
	 ++ concat (map (((:) '\t') . convertStatementToC) (sortDecls statlist))
	 ++ "}\n"

convertStatementToC :: Statement -> String
convertStatementToC (Declare var)
  = "unsigned char " ++ var ++ ";\n"
convertStatementToC (Assign var exp)
  = var ++ " = " ++ (convertExpToC exp) ++ ";\n"
convertStatementToC (Decrease var)
  = var ++ "--;\n"
convertStatementToC (Increase var)
  = var ++ "++;\n"
convertStatementToC (Return exp)
  = "return " ++ (convertExpToC exp) ++ ";\n"

convertExpToC :: Exp -> String
convertExpToC (BinOp op exp1 exp2)
  = "(" ++ (convertExpToC exp1) ++ " " ++ op ++ " " ++ (convertExpToC exp2) ++ ")"
convertExpToC (UnOp "~" exp)
  = "(255 - " ++ (convertExpToC exp) ++ ")"
convertExpToC (Int i)
  = show i
convertExpToC (Var var)
  = var

sortDecls :: StatementList -> StatementList
sortDecls xs
  = [ Declare x | Declare x <- xs ] ++ removeDecls xs

removeDecls :: StatementList -> StatementList
removeDecls ((Declare x) : xs)
  = removeDecls xs
removeDecls (x:xs)
  = x : (removeDecls xs)
removeDecls []
  = []
