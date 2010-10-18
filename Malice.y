{
module Main where
import Scanner
}

%name malice
%tokentype { Token }
%error { parseError }
%token
        sep                               { TSeparator _ }
        declare                           { TDeclare _ }
        assign                            { TAssign _ }
        op                                { TOp _ $$ }
        ret                               { TRet _ }
        var                               { TVar _ $$ }
        int                               { TInt _ $$ }

%%

Program:
        StatementList ret Exp sep         { Program $1 $3 }

StatementList:
        Statement sep                     { StatementList $1 StatementListEmpty }
        | Statement sep StatementList     { StatementList $1 $3 }

Statement:
        var assign Exp                    { Assign $1 $3 }
        | var declare                     { Declare $1 }

Exp:
        int                               { ExpInt $1 }
        | var                             { ExpVar $1 }
        | Exp op Exp                      { ExpOp $2 $1 $3 }


{
parseError :: [Token] -> a
parseError tokenList
    = let pos = tokenPosn(head(tokenList)) 
      in 
        error ("parse error at line " ++ show(getLineNum(pos)) ++ " and column " ++ show(getColumnNum(pos)))

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
  let parseTree = malice (alexScanTokens2 inStr)  
  putStrLn ("parseTree: " ++ show(parseTree))
  print "done"
}
