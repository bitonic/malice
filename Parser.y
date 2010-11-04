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
  declareInt                        { TDeclare IntType _ }
  declareChar                       { TDeclare CharType _ }
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
  ch                                { TChar _ $$ }
  int                               { TInt _ $$ }

%%

-- whatever

Program       : StatementList                 { Program $1 }

StatementList : Statement sep                 { [$1] }
              | Statement sep StatementList   { $1 : $3 }

Statement     : var assign Exp                { Assign $1 $3 }
              | var declareInt                { Declare IntType $1 }
              | var declareChar               { Declare CharType $1 }
              | var "--"                      { Decrease $1 }
              | var "++"                      { Increase $1 }
              | ret Exp                       { Return $2 }

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
              | ch                            { Char $1 }
              | int                           { Int $1 }

{
parseError :: [Token] -> a
parseError tokenList
  = let pos = tokenPosn(head(tokenList)) 
    in 
     error ("Parse error at line " ++ show(getLineNum(pos)) ++
            " and column " ++ show(getColumnNum(pos)))

data Program
     = Program StatementList
     deriving (Show, Eq)

type StatementList = [Statement]

data Statement
     = Assign String Exp
     | Declare MaliceType String
     | Decrease String
     | Increase String
     | Return Exp
     deriving (Show, Eq)

data Exp
     = UnOp String Exp
     | BinOp String Exp Exp
     | Int Int
     | Char Char
     | Var String
     deriving (Show, Eq)
}