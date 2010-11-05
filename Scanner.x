{
module Scanner where
}

%wrapper "monad"

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
  @separator                               { mkT (\l s -> TSeparator) }
  was $white+ a $white+ number             { mkT (\l s -> TDeclare IntType) }
  was $white+ a $white+ letter             { mkT (\l s -> TDeclare CharType) }
  became                                   { mkT (\l s -> TAssign) }
  $operators                               { mkT (\l s -> TOp (take l s)) }
  drank                                    { mkT (\l s -> TOp "--") }
  ate                                      { mkT (\l s -> TOp "++") }
  Alice $white+ found $white+              { mkT (\l s -> TRet) }
  @letter                                  { mkT (\l s -> TChar ((take l s) !! 1)) }
  $alpha [$alpha $digit \_]*               { mkT (\l s -> TVar (take l s)) }
  $digit+                                  { mkT (\l s -> TInt (read (take l s))) }
       

{
mkT :: (Int -> String -> TokenClass) -> AlexInput -> Int -> Alex Token
mkT f (p, _, str) len = return (T p (f len str))              

data Token = T AlexPosn TokenClass
           deriving Show
                    
data MaliceType
     = IntType
     | CharType
     deriving (Eq,Show)
       
data TokenClass
     = TAssign 
     | TDeclare MaliceType 
     | TSeparator 
     | TVar String
     | TChar Char
     | TInt Int
     | TRet 
     | TOp String
     | TEOF
     deriving (Eq,Show)

scanner str = runAlex str $ do
  let loop toks = do tok@(T _ t) <- alexMonadScan; 
                     if t == TEOF
                       then return toks
                       else do loop $! (toks++[tok])
  loop []

alexEOF = return (T undefined TEOF)
}
