{
module Parser where
import Scanner
}

%name maliceParser
%tokentype { Token }
%error { parseError }
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

Program       : StatementList ret Exp1 sep    { Program $1 $3 }

StatementList : Statement sep                 { StatementList $1 StatementListEmpty }
              | Statement sep StatementList   { StatementList $1 $3 }

Statement     : var assign Exp1               { Assign $1 $3 }
              | var declare                   { Declare $1 }

Exp1          : Exp1 "|" Exp2                 { BitOr $1 $3 }
              | Exp1 "^" Exp2                 { BitXor $1 $3 }
              | Exp1 "&" Exp2                 { BitAnd $1 $3 }
              | Exp2                          { Exp2 $1 }

Exp2          : Exp2 "+" Exp3                 { Plus $1 $3 }
              | Exp2 "-" Exp3                 { Minus $1 $3 }
              | Exp3                          { Exp3 $1 }

Exp3          : Exp3 "*" Exp4                 { Times $1 $3 }
              | Exp3 "/" Exp4                 { Div $1 $3 }
              | Exp3 "%" Exp4                 { Mod $1 $3 }
              | Exp4                          { Exp4 $1 }

Exp4          : "~" Term                      { BitNot $2 }
              | Term "--"                     { Decr $1 }
              | Term "++"                     { Incr $1 }
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
    = Program StatementList Exp1
      deriving (Show, Eq)

data StatementList
    = StatementList Statement StatementList
    | StatementListEmpty
    deriving (Show, Eq)

data Statement
    = Assign String Exp1
    | Declare String
    deriving (Show, Eq)

data Exp1
    = BitOr Exp1 Exp2
    | BitXor Exp1 Exp2
    | BitAnd Exp1 Exp2
    | Exp2 Exp2
    deriving (Show, Eq)

data Exp2
    = Plus Exp2 Exp3
    | Minus Exp2 Exp3
    | Exp3 Exp3
    deriving (Show, Eq)

data Exp3
    = Times Exp3 Exp4
    | Div Exp3 Exp4
    | Mod Exp3 Exp4
    | Exp4 Exp4
    deriving (Show, Eq)

data Exp4
    = BitNot Term
    | Decr Term
    | Incr Term
    | Term Term
    deriving (Show, Eq)

data Term
    = Int Int
    | Var String
    deriving (Show, Eq)
}
