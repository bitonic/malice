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
       @separator                               { \s -> Separator }
       was $white+ a $white+ number             { \s -> Declare }
       became                                   { \s -> Assign }
       $operators                               { \s -> Op (head s) }
       drank                                    { \s -> Decrement }
       ate                                      { \s -> Increment }
       Alice $white+ found                      { \s -> Ret }
       $alpha [$alpha $digit \_]*               { \s -> Var s}
       $digit+                                  { \s -> Int (read s) }
       

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
     Assign             |
     Declare            |
     Separator          |
     Var String         |
     Int Int            |
     Increment          |
     Decrement          |
     Ret                |
     Op Char
     deriving (Eq,Show)

main = do
     s <- getContents
     print (alexScanTokens s)
}