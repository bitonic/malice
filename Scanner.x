{
module Scanner where
}

%wrapper "posn"

$digit = 0-9                       -- digits
$alpha = [a-zA-Z]                  -- alphabetic characters
$operators = [\+\-\*\/\%\^\~\&\|]  -- mathematical operators
@separator = and | but | then | \. | \,
@variable = $alpha [$alpha $digit \_]*

tokens :-
       $white+                                  ;
       too                                      ;
       @separator                               { \p s -> TSeparator p }
       was $white+ a $white+ number             { \p s -> TDeclare p }
       became                                   { \p s -> TAssign p }
       $operators                               { \p s -> TOp p s }
       drank                                    { \p s -> TOp p "--" }
       ate                                      { \p s -> TOp p "++" }
       Alice $white+ found $white+              { \p s -> TRet p }
       $alpha [$alpha $digit \_]*               { \p s -> TVar p s}
       $digit+                                  { \p s -> TInt p (read s) }
       

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    TAssign AlexPosn            |
    TDeclare AlexPosn           |
    TSeparator AlexPosn         |
    TVar AlexPosn String        |
    TInt AlexPosn Int           |
    TRet AlexPosn               |
    TOp AlexPosn String
    deriving (Eq,Show)

tokenPosn (TAssign p) = p
tokenPosn (TDeclare p) = p
tokenPosn (TSeparator p) = p
tokenPosn (TVar p s) = p
tokenPosn (TInt p s) = p
tokenPosn (TRet p) = p
tokenPosn (TOp p s) = p

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum 

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum

--alexScanTokens :: String -> [token]
maliceScanner str = go (alexStartPos, '\n', str)
    where go inp@(pos, _, str) =
              case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error ("Lexical error at line " ++ show (getLineNum(pos)) ++ " and column " ++ show (getColumnNum(pos)))
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'


}