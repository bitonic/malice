{
module Scanner where
}

%wrapper "posn"

$digit = 0-9                       -- digits
$alpha = [a-zA-Z]                  -- alphabetic characters
$operators = [\+\-\*\/\%\^\~\&\|]  -- mathematical operators
-- The various separators
@separator = and | but | then | \. | \,
-- Standard variable name, must start with character
-- and can contain characters digits and underscores.
@variable = $alpha [$alpha $digit \_]*
@letter = \' . \'

tokens :-
       $white+                                  ;
       too                                      ;
       @separator                               { \p s -> TSeparator p }
       was $white+ a $white+ number             { \p s -> TDeclare IntType p }
       was $white+ a $white+ letter             { \p s -> TDeclare CharType p }
       became                                   { \p s -> TAssign p }
       $operators                               { \p s -> TOp p s }
       drank                                    { \p s -> TOp p "--" }
       ate                                      { \p s -> TOp p "++" }
       Alice $white+ found $white+              { \p s -> TRet p }
       @letter                                  { \p s -> TChar p (s !! 1) }
       $alpha [$alpha $digit \_]*               { \p s -> TVar p s}
       $digit+                                  { \p s -> TInt p (read s) }
       

{
data MaliceType
     = IntType
     | CharType
     deriving (Eq,Show)
       
data Token
     = TAssign AlexPosn
     | TDeclare MaliceType AlexPosn
     | TSeparator AlexPosn
     | TVar AlexPosn String
     | TChar AlexPosn Char
     | TInt AlexPosn Int
     | TRet AlexPosn
     | TOp AlexPosn String
     deriving (Eq,Show)

tokenPosn (TAssign p) = p
tokenPosn (TDeclare _ p) = p
tokenPosn (TSeparator p) = p
tokenPosn (TVar p _) = p
tokenPosn (TInt p _) = p
tokenPosn (TRet p) = p
tokenPosn (TOp p _) = p

getLineNum (AlexPn _ l c) = l

getColumnNum (AlexPn _ l c) = c

maliceScanner str
  = go (alexStartPos, '\n', str)
  where
    go inp@(pos, _, str)
      = case alexScan inp 0 of
             AlexEOF -> []
             AlexError _ -> error ("Lexical error at line " ++
                                   show (getLineNum pos) ++
                                   " and column " ++ show (getColumnNum pos))
             AlexSkip  inp' len -> go inp'
             AlexToken inp' len act -> act pos (take len str) : go inp'
}