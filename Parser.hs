module Parser
       (
         maliceParser,
         MaliceType(..),
         AST(..), StatementList, Statement(..), Expr(..),
         ASTPos(..), StatementListPos,
         SourcePos(..),
         unPosAST,
       ) where

import Data.Int (Int32)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

-- Abstact Syntax Tree definition
-- The --Pos ones are used in the semantics, so that we
-- can have nice error messages.

data AST = Program StatementList

data ASTPos = ProgramPos StatementListPos
            deriving (Show, Eq)

data MaliceType = MaliceInt | MaliceChar
                deriving (Show, Eq)

type StatementList = [Statement]
type StatementListPos = [(SourcePos, Statement)]

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
     | Int Int32
     | Char Char
     | Var String
     deriving (Show, Eq)
              
-- Converts from AST with positions to a AST without
unPosAST :: ASTPos -> AST              
unPosAST (ProgramPos sl) = Program $ map unPosS sl

unPosS (_, s) = s
              
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
mainparser :: Parser ASTPos
mainparser = do p_white
                sl <- many1 (do {s <- p_statement;
                                 p_separator >> return s})
                return (ProgramPos sl)

p_separator = choice [ p_string "and"
                     , p_string "but"
                     , p_string "then"
                     , p_string "."
                     , p_string ","
                     ]
              <?> "statement separator"


-- Statement
p_statement = do
  pos <- getPosition
  s <- (try p_return
        <|> do { v <- p_identifier;
                 p_statement_id v;
               }
        <?> "statement")
  return (pos, s)

p_return = p_string "Alice found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_declare v)
               <|> p_assign v
                   
p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]
           
p_declare v = do p_string "was a"
                 choice [ p_string "number" >> return (Declare MaliceInt v)
                        , p_string "letter" >> return (Declare MaliceChar v)
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
   <|> liftM Int p_int32

p_int32 = do
  int <- p_integer
  return (fromIntegral int :: Int32)
   
-- Utils   
p_string = p_lexeme . string

-- parser from string
maliceParser :: String -> String -> Either ParseError ASTPos
maliceParser s f = parse mainparser f s