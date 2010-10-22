{
module Parser where
import Scanner
}

%name maliceParser
%tokentype { Token }
%error { parseError }
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

StatementList : Statement sep                 { StatementList $1 StatementListEmpty }
              | Statement sep StatementList   { StatementList $1 $3 }

Statement     : var assign Exp                { Assign $1 $3 }
              | var declare                   { Declare $1 }

Exp           : Exp "|" Exp                   { BitOr $1 $3 }
              | Exp "^" Exp                   { BitXor $1 $3 }
              | Exp "&" Exp                   { BitAnd $1 $3 }
              | Exp "+" Exp                   { Plus $1 $3 }
              | Exp "-" Exp                   { Minus $1 $3 }
              | Exp "*" Exp                   { Times $1 $3 }
              | Exp "/" Exp                   { Div $1 $3 }
              | Exp "%" Exp                   { Mod $1 $3 }
              | "~" Exp                       { BitNot $2 }
              | Exp  "--"                     { Decr $1 }
              | Exp "++"                      { Incr $1 }
              | Term                          { Term $1 }

Term          : int                           { Int $1 }
              | var                           { Var $1 }

{
parseError :: [Token] -> a
parseError tokenList
    = let pos = tokenPosn(head(tokenList)) 
      in 
        error ("Parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

data Program
    = Program StatementList Exp
      deriving (Show, Eq)

data StatementList
    = StatementList Statement StatementList
    | StatementListEmpty
    deriving (Show, Eq)

data Statement
    = Assign String Exp
    | Declare String
    deriving (Show, Eq)

data Exp
    = BitOr Exp Exp
    | BitXor Exp Exp
    | BitAnd Exp Exp
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Mod Exp Exp
    | BitNot Exp
    | Decr Exp
    | Incr Exp
    | Term Term
    deriving (Show, Eq)

data Term
    = Int Int
    | Var String
    deriving (Show, Eq)
}
