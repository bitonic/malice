{
module Main where
import Scanner
}

%name malice
%tokentype { Token }
%error { parseError }

%token
        sep                               { Separator }
        declare                           { Declare }
        '='                               { Assign }
        op                                { Op $$ }
        dec                               { Decrement }
        incr                              { Increment }
        ret                               { Ret }
        var                               { Var $$ }
        int                               { Int $$ }

%%

Program:
        StatementList ret Exp             { Program $1 $3 }

StatementList:
        Statement sep StatementList       { Statement $1 }
        | Statement sep                   { Statement $1 }

Statement:
        var '=' Exp                       { Assign $1 $3 }
        | declare var                     { Assign $2 }
        | var dec                         { Decrement $1 }
        | var incr                        { Increment $1 }

Exp:
        Term                              { Term $1 }
        | Exp op Exp                      { Op $2 $1 $3 }

Term:
        int                               { Int $1 }
        | var                             { Var $1 }        

