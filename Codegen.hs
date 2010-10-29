module Main where

import System ( getArgs )
import Parser
import Scanner
import List

main = do 
  [fn] <- getArgs
  f <- readFile fn
  let code = simplifyProgram $ maliceParser $ maliceScanner f
  putStrLn $ show code
  putStrLn $ convertProgramToC code
--  putStrLn $ show (llProgram code)



-- Always from right to left, Intel-style

data LLcmd
--Copy Dest Src
     = LLCpRegVar Int String
	 | LLCpVarReg String Int
	 | LLCpRegImm Int Int
--Set Dest value
	 | LLAdd Int Int
	 | LLMul Int Int
--Return: Have the return value ready in register 0!
	 | LLRet
     deriving (Show, Eq)



llProgram :: Program -> [LLcmd]
llProgram (Program statlist retexp)
  = (llStatlist statlist (0, 4))
	++ (llExp retexp (0, 4)) ++ [LLRet]

llStatlist :: StatementList -> (Int, Int) -> [LLcmd]
llStatlist ((Declare var) : ss) (destreg, maxreg)
  = llStatlist ss (destreg, maxreg)
llStatlist ((Assign var exp) : ss) (destreg, maxreg)
  = (llExp exp (destreg, maxreg)) ++ [(LLCpVarReg var destreg)]
	++ (llStatlist ss (destreg, maxreg))
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



convertProgramToC :: Program -> String
convertProgramToC (Program statlist retval)
  = "int main()\n{\n"
	 ++ concat (map (((:) '\t') . convertStatementToC) statlist)
	 ++ "\n\treturn " ++ (convertExpToC retval)
	 ++ ";\n}\n"

convertStatementToC :: Statement -> String
convertStatementToC (Declare var)
  = "unsigned char " ++ var ++ ";\n"
convertStatementToC (Assign var exp)
  = var ++ " = " ++ (convertExpToC exp) ++ ";\n"

convertExpToC :: Exp -> String
convertExpToC (BinOp op exp1 exp2)
  = "(" ++ (convertExpToC exp1) ++ " " ++ op ++ " " ++ (convertExpToC exp2) ++ ")"
convertExpToC (UnOp "~" exp)
  = "(255 - " ++ (convertExpToC exp) ++ ")"
convertExpToC (Int i)
  = show i
convertExpToC (Var var)
  = var




simplifyProgram :: Program -> Program
simplifyProgram (Program statlist retval)
  = (Program (sortDecls statlist) retval)

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
