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
                | MaliceString
                | MaliceArray MaliceType Int32
                deriving (Show, Eq)
                        
type StatementList = [Statement]

type Statement = (SourcePos, StatementAct)
data StatementAct
     = Assign Identifier Expr
     | Declare MaliceType String
     | DeclareArray String MaliceType Expr
     | Decrease Identifier
     | Increase Identifier
     | Return Expr
     | Print Expr
     | Get Identifier
     | ProgramDoc String
     | ChangerCall String Identifier
     -- Composite statements
     | Until Expr StatementList
     | IfElse [(Expr, StatementList)]
     | Function String FunctionArgs MaliceType StatementList
     | Changer String MaliceType StatementList
     deriving (Show, Eq)

type FunctionArgs = [(String, MaliceType)]

data Identifier = Single String
                | Array String Expr -- Name position
                deriving (Show, Eq)

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | FunctionCall String [Expr]
     | Int Int32
     | Char Char
     | String String
     | Var Identifier
     deriving (Show, Eq)
              
-- Language characteristics

operators = ["+", "-", "*", "/", "%", "^", "&", "|", "~",
             "==", "<", ">", ">=", "<=", "&&", "||", "!="
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
TokenParser { identifier = p_varName
            , reservedOp = p_reservedOp
            , integer = p_integer
            , natural = p_natural
            , whiteSpace = p_white
            , charLiteral = p_letter
            , parens = p_parens
            , lexeme = p_lexeme
            } = makeTokenParser def

p_identifier = try p_arrayEl
               <|> liftM Single p_varName
               <?> "identifier"
p_arrayEl = do
  id <- p_varName
  p_string "'s"
  pos <- p_expr
  p_string "piece"
  return (Array id pos)

-- Actual parser
mainparser :: Parser StatementList
mainparser = p_white >> manyTill p_statement eof

p_separator = try (p_string "too" >> p_separator')
              <|> p_separator'
              <?> "statement separator"
  where p_separator' = choice $ map p_string [ "and", "but", "then", ".", ","]

-- Statement
p_statement = do
  pos <- getPosition
  s <- (try (p_return <* p_separator)
        <|> try ((p_varName >>= p_declare) <* p_separator)
        <|> try ((p_varName >>= p_declarearray) <* p_separator)
        <|> try ((p_identifier >>= p_incdec) <* p_separator)
        <|> try ((p_identifier >>= p_assign) <* p_separator)
        <|> try (p_print <* p_separator)
        <|> try (p_get <* (p_separator <|> p_string "?"))
        <|> try (p_programdoc <* p_separator)
        <|> try (p_until <* p_separator)
        <|> try (p_ifelse <* p_separator)
        <|> try (p_changercall <* p_separator)
        <|> try p_function
        <|> p_changer
        <?> "statement")
  return (pos, s)

p_return = p_cstring "Alice found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_assign v)
               <?> "assignment or decrease/increase"

p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]

p_declare v = do
  p_cstring "was a"
  liftM ((flip Declare) v) p_type

p_declarearray v = do
  p_string "had"
  size <- p_expr
  t <- p_type
  return (DeclareArray v t size)
  
p_assign v = p_string "became" >> liftM (Assign v) p_expr

p_print = liftM Print (p_expr <*
                       (try (p_cstring "spoke")
                        <|> p_cstring "said Alice"))

p_get = p_cstring "what was" >> liftM Get p_identifier

p_programdoc = liftM ProgramDoc (p_quotedstring <* p_cstring "thought Alice")
          
-- Composite statements
p_until = do
  p_string "eventually"
  e <- p_parens p_expr
  p_string "because"
  liftM (Until e) $ manyTill p_statement $ try (p_cstring "enough times")

p_ifelse = do 
  (p_string "perhaps" <|> p_string "either")
  e <- p_expr
  p_string "so"
  s <- many p_statement
  rest <- manyTill elseifs $ try (
    try (p_string "or" >> notFollowedBy (p_string "maybe" >> return 'x'))
    <|> end)
  finalOr <- optionMaybe (manyTill p_statement (try end))
  case finalOr of
    Just s' -> return $ IfElse $ [(e, s)] ++ rest ++ [(Int 1, s')]
    Nothing -> return $ IfElse $ (e, s) : rest
  where
    elseifs =
      liftM2 (,) (p_cstring "or maybe" >> (p_expr <* p_string "so")) (many p_statement)
    end = p_cstring "Alice was unsure" >> optional (p_string "which")

p_function = do
  p_white
  p_cstring "The room"
  name <- p_varName
  args <-  p_parens $ sepBy (liftM2 (flip (,)) p_type p_varName) (p_string ",")
  p_cstring "contained a"
  ret <- p_type
  sl <- manyTill p_statement p_nextfunction
  return $ Function name args ret sl

p_changer = do
  p_white
  p_cstring "The Looking-Glass"
  name <- p_varName
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
  liftM ((flip ChangerCall) var) p_varName

p_type = liftM stringToType (p_string "number"
                             <|> p_string "letter"
                             <|> p_string "sentence"
                             <?> "type")
         
-- Expression
p_expr = buildExpressionParser table term <?> "expression"
table = [ [prefixOp "~"]
        , map infixOp ["*", "/", "%"]
        , map infixOp ["+", "-"]
        , map infixOp ["<", ">", ">=", "<="]
        , map infixOp ["==", "!="]
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
       <|> try (do { f <- p_varName;
                     liftM (FunctionCall f) (p_parens $ sepBy p_expr (p_string ","));
                   })
       <|> liftM String p_quotedstring
       <|> liftM Var p_identifier
       <|> liftM Char p_letter
       <|> liftM Int p_int32

p_quotedstring = p_string "\"" >> manyTill anyChar (p_string "\"")

p_int32 = do
  int <- p_integer
  return (fromIntegral int :: Int32)
  
p_operator = choice $ map p_string operators

-- Utils
p_string = p_lexeme . string

p_cstring = mapM p_string . words

p <* q = p >>= (\x -> q >> return x)

stringToType "number" = MaliceInt
stringToType "letter" = MaliceChar
stringToType "sentence" = MaliceString

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
