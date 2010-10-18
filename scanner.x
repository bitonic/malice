{
module Scanner where
}

%wrapper "basic"

$digit = 0-9                       -- digits
$alpha = [a-zA-Z]                  -- alphabetic characters
$operators = [\+\-\*\/\%\^\~]      -- mathematical operators
@separator = and | but | then | \. | \,
@variable = $alpha [$alpha $digit \_]*

tokens :-
       $white+                                  ;
       too                                      ;
       @separator                               { \s -> TSeparator }
       was $white+ a $white+ number             { \s -> TDeclare }
       became                                   { \s -> TAssign }
       $operators                               { \s -> TOp s }
       drank                                    { \s -> TOp "--" }
       ate                                      { \s -> TOp "++" }
       Alice $white+ found                      { \s -> TRet }
       $alpha [$alpha $digit \_]*               { \s -> TVar s}
       $digit+                                  { \s -> TInt (read s) }
       

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    TAssign             |
    TDeclare            |
    TSeparator          |
    TVar String         |
    TInt Int            |
    TRet                |
    TOp String
    deriving (Eq,Show)
}