module Code2C where

import Parser
import CodeCleanup

-- FIXME: Declares ALL variables as u8


convertProgramToC :: AST -> String
convertProgramToC (Program statlist)
  = "int main()\n{\n"
    ++ concatMap ((:) '\t' . convertStatementToC) (sortDecls statlist)
    ++ "}\n"

convertStatementToC :: Statement -> String
convertStatementToC (Declare MaliceChar var)
  = "unsigned char " ++ var ++ ";\n"
convertStatementToC (Declare MaliceInt var)
  = "int " ++ var ++ ";\n";
convertStatementToC (Assign var exp)
  = var ++ " = " ++ convertExpToC exp ++ ";\n"
convertStatementToC (Decrease var)
  = var ++ "--;\n"
convertStatementToC (Increase var)
  = var ++ "++;\n"
convertStatementToC (Return exp)
  = "return " ++ convertExpToC exp ++ ";\n"

convertExpToC :: Expr -> String
convertExpToC (BinOp op exp1 exp2)
  = "(" ++ convertExpToC exp1 ++ " " ++ op ++ " " ++ convertExpToC exp2 ++ ")"
convertExpToC (UnOp "~" exp)
  = "(~" ++ convertExpToC exp ++ ")"
convertExpToC (Int i)
  = show i
convertExpToC (Var var)
  = var
