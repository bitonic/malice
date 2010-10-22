{
module Parser where
import Scanner
}

%name maliceParser
%tokentype { Token }
%error { parseError }
-- Operator priority
%left "|"
%left "^"
%left "&"
%left "+" "-"
%left "*" "/" "%"
%left "~" "--" "++"
%token
  sep                               { TSeparator _ }
  declare                           { TDeclare _ }
  assign                            { TAssign _ }
  "+"                               { TOp _ "+" }
  "-"                               { TOp _ "-" }
  "*"                               { TOp _ "*" }
  "/"                               { TOp _ "/" }
  "%"                               { TOp _ "%" }
  "^"                               { TOp _ "^" }
  "~"                               { TOp _ "~" }
  "&"                               { TOp _ "&" }
  "|"                               { TOp _ "|" }
  "--"                              { TOp _ "--" }
  "++"                              { TOp _ "++" }
  ret                               { TRet _ }
  var                               { TVar _ $$ }
  int                               { TInt _ $$ }

%%

Program       : StatementList ret Exp sep     { Program $1 $3 }

StatementList : Statement sep                 { [$1] }
              | Statement sep StatementList   { $1 : $3 }

Statement     : var assign Exp                { Assign $1 $3 }
              | var declare                   { Declare $1 }
              | var "--"
                {
                  Assign $1 (BinOp "+" (Var $1) (Int 1))
                }
              | var "++"
                {
                  Assign $1 (BinOp "-" (Var $1) (Int 1))
                }

Exp           : Exp "|" Exp                   { BinOp "|" $1 $3 }
              | Exp "^" Exp                   { BinOp "^" $1 $3 }
              | Exp "&" Exp                   { BinOp "&" $1 $3 }
              | Exp "+" Exp                   { BinOp "+" $1 $3 }
              | Exp "-" Exp                   { BinOp "-" $1 $3 }
              | Exp "*" Exp                   { BinOp "*" $1 $3 }
              | Exp "/" Exp                   { BinOp "/" $1 $3 }
              | Exp "%" Exp                   { BinOp "%" $1 $3 }
              | "~" Exp                       { UnOp "~" $2 }
              | var                           { Var $1 }
              | int                           { Int $1 }

{
parseError :: [Token] -> a
parseError tokenList
  = let pos = tokenPosn(head(tokenList)) 
    in 
     error ("Parse error at line " ++ show(getLineNum(pos)) ++
            " and column " ++ show(getColumnNum(pos)))

data Program
     = Program StatementList Exp
     deriving (Show, Eq)

type StatementList = [Statement]

data Statement
     = Assign String Exp
     | Declare String
     deriving (Show, Eq)

data Exp
     = UnOp String Exp
     | BinOp String Exp Exp
     | Int Int
     | Var String
     deriving (Show, Eq)
}
