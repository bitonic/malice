module Parser
       (
         maliceParser, maliceParseFile
       ) where

import Common
import Data.Int ( Int32 )
import Data.Map ( empty )
import Control.Monad ( liftM, liftM2 )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
--import Text.ParserCombinators.Parsec.Pos ( sourceLine, sourceColumn )

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
               <|> liftM SingleElement p_varName
               <?> "identifier"
p_arrayEl = do
  var <- p_varName
  _ <- p_string "'s"
  pos <- p_expr
  _ <- p_string "piece"
  return (ArrayElement var pos)

-- Actual parser
mainparser :: String -> Parser AST
mainparser f = do
  p_white 
  sl <- manyTill p_statement (try $ lookAhead ((p_cstring "The" >> return ()) <|> eof))
  ds <- manyTill p_declaration eof
  return (AST f (((0,0), Function empty mainFunction [] MaliceInt sl) : ds))

p_separator = try (p_string "too" >> p_separator')
              <|> p_separator'
              <?> "statement separator"
  where p_separator' = choice $ map p_string [ "and", "but", "then", ".", ",", "?"]

-- Statement
p_statement = do
  p <- getPosition
  s <- (try (p_return <* p_separator)
        <|> try ((p_varName >>= p_declare) <* p_separator)
        <|> try ((p_varName >>= p_declarearray) <* p_separator)
        <|> try ((p_identifier >>= p_incdec) <* p_separator)
        <|> try ((p_identifier >>= p_assign) <* p_separator)
        <|> try (p_print <* p_separator)
        <|> try (p_get <* p_separator)
        <|> try (p_comment <* p_separator)
        <|> try (p_until <* p_separator)
        <|> try (p_ifelse <* p_separator)
        <|> try (p_changercall <* p_separator)
        <|> try (liftM FunctionCallS p_functioncall <* p_separator)
        <?> "statement")
  return ((sourceLine p, sourceColumn p), s)

-- Declaration statement
p_declaration = do
  p <- getPosition
  d <- (try p_function <|> p_changer)
  return ((sourceLine p, sourceColumn p), d)

p_return = p_cstring "Alice found" >> liftM Return p_expr

p_statement_id v = try (p_incdec v)
               <|> try (p_assign v)
               <?> "assignment or decrease/increase"

p_incdec v = choice [ p_string "ate" >> return (Increase v)
                    , p_string "drank" >> return (Decrease v)
                    ]

p_declare v = do
  _ <- p_cstring "was a"
  liftM (flip Declare v) p_type

p_declarearray v = do
  _ <- p_string "had"
  size <- p_expr
  t <- p_type
  return (Declare (MaliceArraySize t size) v)
  
p_assign v = p_string "became" >> liftM (Assign v) p_expr

p_print = liftM Print (p_expr <*
                       (try (p_cstring "spoke")
                        <|> p_cstring "said Alice"))

p_get = p_cstring "what was" >> liftM Get p_identifier

p_comment = liftM Comment (p_quotedstring <* p_cstring "thought Alice")
          
-- Composite statements
p_until = do
  _ <- p_string "eventually"
  e <- p_parens p_expr
  _ <- p_string "because"
  liftM (Until empty e) $ manyTill p_statement $ try (p_cstring "enough times")

p_ifelse = do 
  _ <- (p_string "perhaps" <|> p_string "either")
  e <- p_expr
  _ <- p_string "so"
  s <- many p_statement
  rest <- manyTill elseifs $ try (
    try (p_string "or" >> notFollowedBy (p_string "maybe" >> return 'x'))
    <|> end)
  finalOr <- optionMaybe (manyTill p_statement (try end))
  case finalOr of
    Just s' -> return $ IfElse $ [(empty, e, s)] ++ rest ++ [(empty, Int 1, s')]
    Nothing -> return $ IfElse $ (empty, e, s) : rest
  where
    elseifs =
      liftM2 ((,,) empty) (p_cstring "or maybe" >> (p_expr <* p_string "so")) (many p_statement)
    end = p_cstring "Alice was unsure" >> optional (p_string "which")

p_function = do
  p_white
  _ <- p_cstring "The room"
  name <- p_varName
  args <-  p_parens $ sepBy (liftM2 (flip (,)) p_type p_varName) (p_string ",")
  _ <- p_cstring "contained a"
  ret <- p_type
  sl <- manyTill p_statement p_nextfunction
  return $ Function empty name args ret sl

p_changer = do
  p_white
  _ <- p_cstring "The Looking-Glass"
  name <- p_varName
  _ <- p_cstring "changed a"
  t <- p_type
  sl <- manyTill (lookAhead (notFollowedBy (p_return >> return 'x')) >> p_statement) p_nextfunction
  return $ Function empty name [("it", t)] t (sl ++
                                              [((0,0), Return (Id (SingleElement "it")))])
  
p_nextfunction =
  eof
  <|> (try (lookAhead (p_white >> p_string "The")) >> return ())
  <?> "function or Looking-Glass declaration"

p_changercall = do
  var <- p_identifier
  _ <- p_cstring "went through"
  function <- p_varName
  return (Assign var (FunctionCall function [Id var]))

p_type =
  try (liftM MaliceArray (p_string "spider" >> p_type'))
  <|> p_type'
  where
    p_type' = liftM stringToType (p_string "number"
                                  <|> p_string "letter"
                                  <|> p_string "sentence"
                                  <?> "type")
         
-- Expression
p_expr = buildExpressionParser table term <?> "expression"
table = [ map prefixOp ["~", "-"]
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
       <|> try p_functioncall
       <|> liftM String p_quotedstring
       <|> liftM Id p_identifier
       <|> liftM Char p_letter
       <|> liftM Int p_int32

p_functioncall = do
  f <- p_varName
  liftM (FunctionCall f) (p_parens $ sepBy p_expr p_separator);

p_quotedstring = p_string "\"" >> manyTill anyChar (p_string "\"")

p_int32 = do
  int <- p_natural
  return (fromIntegral int :: Int32)
  
p_operator = choice $ map p_string operators

-- Utils
p_string = p_lexeme . string

p_cstring = mapM p_string . words

p <* q = p >>= (\x -> q >> return x)

-- useful for debugging when you don't want the positions
showSL :: StatementList -> String
showSL = show . map snd

-- parser from string
maliceParser :: String -> String -> Either ParseError AST
maliceParser s f = parse (mainparser f) f s

-- Parse from file
maliceParseFile :: String -> IO (Either ParseError AST)
maliceParseFile f = do
  s <- readFile f
  case maliceParser s f of
    (Right sl) -> return (Right sl)
    (Left err) -> return (Left err)
