module Parser
       (
         maliceParser, maliceParseFile,
         MaliceType(..),
         StatementList, Statement(..), Expr(..),
         SourcePos, newPos,
       ) where

import Data.Int ( Int32 )
import Control.Monad ( liftM, liftM2 )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos ( newPos )

-- Abstact Syntax Tree definition
-- The --Pos ones are used in the semantics, so that we
-- can have nice error messages.

data MaliceType = MaliceInt | MaliceChar | MaliceBool
                deriving (Show, Eq)

type StatementList = [Statement]

type Statement = (SourcePos, StatementAct)
data StatementAct
     = Assign String Expr
     | Declare MaliceType String
     | Decrease String
     | Increase String
     | Return Expr
     -- Composite statements
     | Until Expr StatementList
     | Switch [(Expr, Statement)]
     deriving (Show, Eq)

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | Int Int32
     | Char Char
     | Var String
     deriving (Show, Eq)
              
-- Language characteristics

operators = ["+", "-", "*", "/", "%", "^", "&", "|", "~",
             "==", "<", ">", "&&", "||"
            ]
 

def = emptyDef { identStart = letter
               , identLetter = alphaNum <|> char '_'
               , opStart = oneOf $ concat $ map (\o -> [head o]) operators
               , opLetter = oneOf $ concat operators
               , reservedOpNames = operators
               , reservedNames = ["and", "but", "then", ".",
                                  "too", "Alice", "found", "perhaps",
                                  "maybe", "eventually", "because"]
               }

-- Generate useful parsers with makeTokenParser                 
TokenParser { identifier = p_identifier
            , reservedOp = p_reservedOp
            , integer = p_integer
            , whiteSpace = p_white
            , lexeme = p_lexeme
            , charLiteral = p_letter
            , parens = p_parens
            } = makeTokenParser def

-- Actual parser
mainparser :: Parser StatementList
mainparser = do p_white
                sl <- many1 p_statement
                return sl

p_separator s = s <* (try (p_string "too" >> p_separator')
                      <|> p_separator'
                      <?> "statement separator")
  where p_separator' = choice $ map p_string [ "and", "but", "then", ".", ","]

-- Statement
p_statement = do
  pos <- getPosition
  s <- (try p_return
        <|> try (p_separator $ p_identifier >>= p_statement_id)
        <|> try (p_separator $ p_until)
        <|> p_switch
        <?> "statement")
  return (pos, s)

p_return = p_string "Alice" >> p_string "found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_declare v)
               <|> p_assign v

p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]

p_declare v = do
  p_string "was"
  p_string "a"
  choice [ p_string "number" >> return (Declare MaliceInt v)
         , p_string "letter" >> return (Declare MaliceChar v)
         ]

p_assign v = p_string "became" >> liftM (Assign v) p_expr

-- Composite statements
p_until = do
  p_string "eventually"
  e <- p_expr
  p_string "because"
  liftM (Until e) $ manyTill p_statement (try (p_string "enough" >>
                                               p_string "times"))

p_switch = do 
  p_string "perhaps"
  e <- p_expr
  p_string "so"
  s <- p_statement
  liftM (Switch . (:) (e, s)) $ many p_switch'
  where p_switch' = do
          p_string "or" >> p_string "maybe"
          liftM2 (,) (p_expr <* p_string "so") p_statement

-- Expression
p_expr = buildExpressionParser table term <?> "expression"
table = [ [prefixOp "~"]
        , map infixOp ["*", "/", "%"]
        , map infixOp ["+", "-"]
        , map infixOp ["<", ">"]
        , [infixOp "=="]
        , [infixOp "&"]
        , [infixOp "^"]
        , [infixOp "|"]
        , [infixOp "&&"]
        , [infixOp "||"]
        ]

prefixOp op
  = Prefix (p_reservedOp op >> return (UnOp op))
    
infixOp op
  = Infix (p_reservedOp op >> return (BinOp op)) AssocLeft

term = (lookAhead p_operator >> p_expr)
       <|> p_parens p_expr
       <|> liftM Var p_identifier
       <|> liftM Char p_letter
       <|> liftM Int p_int32

p_int32 = do
  int <- p_integer
  return (fromIntegral int :: Int32)
  
p_operator = choice $ map p_string operators

-- Utils
p_string = p_lexeme . string

p <* q = p >>= (\x -> q >> return x)

-- useful for debugging when you don't want the positions
showSL :: StatementList -> String
showSL = show . map snd

-- parser from string
maliceParser :: String -> String -> Either ParseError StatementList
maliceParser s f = parse mainparser f s

-- Parse from file
maliceParseFile :: String -> IO (Either ParseError StatementList)
maliceParseFile f = do
  s <- readFile f
  case maliceParser s f of
    (Right sl) -> return (Right sl)
    (Left err) -> return (Left err)