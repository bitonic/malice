module Code2C where

import Parser
import CodeCleanup



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
