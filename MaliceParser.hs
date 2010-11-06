module MaliceParser
       (
         mainparser
       ) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

-- Abstact Syntax Tree definition

data AST = Program [Statement]
         deriving (Show, Eq)

data MaliceType = Int32 | Char8
                deriving (Show, Eq)

data Statement
     = Assign String Expr
     | Declare MaliceType String
     | Decrease String
     | Increase String
     | Return Expr
     deriving (Show, Eq)
                        
data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | Int Integer
     | Char Char
     | Var String
     deriving (Show, Eq)

-- Language characteristics

operators = "+-*/%^&|"

def = emptyDef { identStart = letter
               , identLetter = alphaNum <|> char '_'
               , opStart = oneOf operators
               , opLetter = oneOf operators
               , reservedOpNames = [[op] | op <- operators]
               }
-- Generate useful parsers with makeTokenParser                 
TokenParser { identifier = p_identifier
            , operator = p_operator
            , reservedOp = p_reservedOp
            , integer = p_integer
            , whiteSpace = p_white
            , parens = p_parens
            , lexeme = p_lexeme
            } = makeTokenParser def

-- Actual parser
mainparser :: Parser AST
mainparser = do p_white
                sl <- many1 (do {s <- p_statement;
                                 p_separator >> return s})
                return (Program sl)

p_separator = choice [ p_string "and"
                     , p_string "but"
                     , p_string "then"
                     , p_string "."
                     , p_string ","
                     ]
              <?> "statement separator"


-- Statement
p_statement = try p_return
          <|> do { v <- p_identifier;
                   p_statement_id v;
                 }
          <?> "statement"

p_return = p_string "Alice found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_declare v)
               <|> p_assign v
                   
p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]
           
p_declare v = do p_string "was a"
                 choice [ p_string "number" >> return (Declare Int32 v)
                        , p_string "letter" >> return (Declare Char8 v)
                        ]
                
p_assign v = p_string "became" >> liftM (Assign v) p_expr

-- Expression
p_expr = buildExpressionParser table term <?> "expression"
table = [ [prefixOp "~"]
        , map infixOp ["*", "/", "%"]
        , map infixOp ["+", "-"]
        , [infixOp "&"]
        , [infixOp "^"]
        , [infixOp "|"]
        ]

prefixOp op
  = Prefix (p_reservedOp op >> return (UnOp op))
    
infixOp op    
  = Infix (p_reservedOp op >> return (BinOp op)) AssocLeft
    
term = (lookAhead p_operator >> p_expr)
   <|> liftM Var p_identifier
   <|> liftM Int p_integer
   
-- Utils   
p_string = p_lexeme . string