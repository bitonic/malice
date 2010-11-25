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

data MaliceType = MaliceInt
                | MaliceChar
                | MaliceArray MaliceType Int32
                deriving (Show, Eq)
                        
type StatementList = [Statement]

type Statement = (SourcePos, StatementAct)
data StatementAct
     = Assign String Expr
     | AssignArray Expr Expr
     | Declare MaliceType String
     | DeclareArray String MaliceType Expr
     | Decrease String
     | Increase String
     | Return Expr
     | PrintString String
     | PrintExpr Expr
     | Get String
     | ProgramDoc String
     | ChangerCall String String
     -- Composite statements
     | Until Expr StatementList
     | IfElse [(Expr, StatementList)]
     | Function String FunctionArgs MaliceType StatementList
     | Changer String MaliceType StatementList
     deriving (Show, Eq)

type FunctionArgs = [(String, MaliceType)]

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | FunctionCall String [Expr]
     | Int Int32
     | Char Char
     | Var String
     | Array String Expr
     deriving (Show, Eq)
              
-- Language characteristics

operators = ["+", "-", "*", "/", "%", "^", "&", "|", "~",
             "==", "<", ">", "&&", "||"
            ]
 

def = emptyDef { identStart = letter
               , identLetter = alphaNum <|> char '_'
               , opStart = oneOf $ concatMap (\o -> [head o]) operators
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
            , natural = p_natural
            , whiteSpace = p_white
            , charLiteral = p_letter
            , parens = p_parens
            , lexeme = p_lexeme
            } = makeTokenParser def

-- Actual parser
mainparser :: Parser StatementList
mainparser = p_white >> many1 p_statement

p_separator = try (p_string "too" >> p_separator')
              <|> p_separator'
              <?> "statement separator"
  where p_separator' = choice $ map p_string [ "and", "but", "then", ".", ","]

-- Statement
p_statement = do
  pos <- getPosition
  s <- (    try (p_return <* p_separator)
        <|> try ((p_identifier >>= p_statement_id) <* p_separator)
        <|> try (p_assignarray <* p_separator)
        <|> try (p_printstring <* p_separator)
        <|> try (p_printexpr <* p_separator)
        <|> try (p_get <* p_string "?")
        <|> try (p_programdoc <* p_separator)
        <|> try (p_until <* p_separator)
        <|> try (p_ifelse <* p_separator)
        <|> try p_function
        <|> try p_changer
        <|> p_changercall <* p_separator
        <?> "statement")
  return (pos, s)

p_return = p_cstring "Alice found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_declare v)
               <|> try (p_assign v)
               <|> p_declarearray v

p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]

p_declare v = do
  p_cstring "was a"
  choice [ p_string "number" >> return (Declare MaliceInt v)
         , p_string "letter" >> return (Declare MaliceChar v)
         ]

p_declarearray v = do
  p_string "had"
  size <- p_expr
  t <- p_type
  return (DeclareArray v t size)
  
p_assign v = p_string "became" >> liftM (Assign v) p_expr

p_assignarray = do
  arr <- p_arrayel
  p_string "became"
  liftM (AssignArray arr) p_expr
  
p_printstring = liftM PrintString (p_quotedstring <*
                                   (try (p_cstring "said Alice")
                                    <|> p_cstring "spoke"))

p_printexpr = liftM PrintExpr (p_expr <* p_cstring "spoke")

p_get = p_cstring "what was" >> liftM Get p_identifier

p_programdoc = liftM ProgramDoc (p_quotedstring <* p_cstring "thought Alice")
          
-- Composite statements
p_until = do
  p_string "eventually"
  e <- p_expr
  p_string "because"
  liftM (Until e) $ manyTill p_statement $ try (p_cstring "enough times")

p_ifelse = do 
  p_string "perhaps"
  e <- p_expr
  p_string "so"
  s <- many p_statement
  liftM (IfElse . (:) (e, s)) $ manyTill elseifs $
    try (p_cstring "Alice was unsure" >>
         optional (p_string "which"))
  where
    elseifs = do
      p_cstring "or maybe"
      liftM2 (,) (p_expr <* p_string "so") (many p_statement)

p_function = do
  p_white
  p_cstring "The room"
  name <- p_identifier
  args <-  p_parens $ sepBy (liftM2 (flip (,)) p_type p_identifier) (p_string ",")
  p_cstring "contained a"
  ret <- p_type
  sl <- manyTill p_statement p_nextfunction
  return $ Function name args ret sl

p_changer = do
  p_white
  p_cstring "The Looking-Glass"
  name <- p_identifier
  p_cstring "changed a"
  t <- p_type
  liftM (Changer name t) $ manyTill p_statement p_nextfunction 
  
p_nextfunction =
  eof
  <|> (try (lookAhead (p_white >> p_string "The")) >> return ())
  <?> "function or Looking-Glass declaration"

p_changercall = do
  var <- p_identifier
  p_cstring "went through"
  liftM ((flip ChangerCall) var) p_identifier

p_type = liftM stringToType (p_string "number"
                             <|> p_string "letter"
                             <?> "type")
         
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
       <|> try (do { f <- p_identifier;
                     liftM (FunctionCall f) (p_parens $ sepBy p_expr (p_string ","));
                   })
       <|> try p_arrayel
       <|> liftM Var p_identifier
       <|> liftM Char p_letter
       <|> liftM Int p_int32

p_arrayel = do
  id <- p_identifier
  p_string "'s"
  pos <- p_expr
  p_string "piece"
  return (Array id pos)

p_int32 = do
  int <- p_integer
  return (fromIntegral int :: Int32)
  
p_operator = choice $ map p_string operators

-- Utils
p_string = p_lexeme . string

p_cstring = mapM p_string . words

p_quotedstring = p_string "\"" >> manyTill anyChar (p_string "\"")

p <* q = p >>= (\x -> q >> return x)

stringToType "number" = MaliceInt
stringToType "letter" = MaliceChar

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
