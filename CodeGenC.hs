module CodeGenC where

import Parser

codeGenC :: Program -> String
codeGenC (Program sl e)
    = "#include <stdio.h>\n" ++
      "int main(){" ++
      codeGenCSl sl ++
      "printf(\"Program returned: %u\\n\"," ++
      codeGenCE1 e ++
      ");return(0);}"

codeGenCSl :: StatementList -> String
codeGenCSl (StatementList s sl)
    = codeGenCS s ++ ";" ++ codeGenCSl sl
codeGenCSl StatementListEmpty
    = ""

codeGenCS :: Statement -> String
codeGenCS (Assign s e)
    = s ++ "=" ++ (codeGenCE1 e)
codeGenCS (Declare s)
    = "unsigned char " ++ s

codeGenCE1 :: Exp1 -> String
codeGenCE1 (BitOr e1 e2)
    = codeGenCE1 e1 ++ "|" ++ (codeGenCE2 e2)
codeGenCE1 (BitXor e1 e2)
    = codeGenCE1 e1 ++ "^" ++ (codeGenCE2 e2)
codeGenCE1 (BitAnd e1 e2)
    = codeGenCE1 e1 ++ "&" ++ (codeGenCE2 e2)
codeGenCE1 (Exp2 e2)
    = codeGenCE2 e2

codeGenCE2 :: Exp2 -> String
codeGenCE2 (Plus e2 e3)
    = codeGenCE2 e2 ++ "+" ++ (codeGenCE3 e3)
codeGenCE2 (Minus e2 e3)
    = codeGenCE2 e2 ++ "-" ++ (codeGenCE3 e3)
codeGenCE2 (Exp3 e3)
    = codeGenCE3 e3

codeGenCE3 :: Exp3 -> String
codeGenCE3 (Times e3 e4)
    = codeGenCE3 e3 ++ "*" ++ (codeGenCE4 e4)
codeGenCE3 (Div e3 e4)
    = codeGenCE3 e3 ++ "/" ++ (codeGenCE4 e4)
codeGenCE3 (Mod e3 e4)
    = codeGenCE3 e3 ++ "%" ++ (codeGenCE4 e4)
codeGenCE3 (Exp4 e4)
    = codeGenCE4 e4

codeGenCE4 :: Exp4 -> String
codeGenCE4 (BitNot t)
    = "~" ++ codeGenCT t
codeGenCE4 (Decr t)
    = codeGenCT t ++ "--"
codeGenCE4 (Incr t)
    = codeGenCT t ++ "++"
codeGenCE4 (Term t)
    = codeGenCT t

codeGenCT :: Term -> String
codeGenCT (Int i)
    = show i
codeGenCT (Var s)
    = s