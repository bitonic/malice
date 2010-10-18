{
module Main where
import Scanner
}

%name malice
%tokentype { Token }
%error { parseError }
%token
        sep                               { TSeparator }
        declare                           { TDeclare }
        '='                               { TAssign }
        op                                { TOp $$ }
        ret                               { TRet }
        var                               { TVar $$ }
        int                               { TInt $$ }

%%

Program:
        StatementList ret Exp             { Program $1 $3 }

StatementList:
        Statement sep                     { StatementList $1 StatementListEmpty }
        | Statement sep StatementList     { StatementList $1 $3 }

Statement:
        var '=' Exp                       { Assign $1 $3 }
        | declare var                     { Declare $2 }

Exp:
        int                               { ExpInt $1 }
        | var                               { ExpVar $1 }
        | Exp op Exp                      { ExpOp $2 $1 $3 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

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
    = ExpInt Int
    | ExpVar String
    | ExpOp String Exp Exp
    deriving (Show, Eq)

main = do 
  inStr <- getContents
  let parseTree = malice (alexScanTokens inStr)  
  putStrLn ("parseTree: " ++ show(parseTree))
  print "done"
}
