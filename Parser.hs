module Parser
       (
         maliceParser, maliceParseFile
       ) where

import Common
import Data.Char ( isSpace )
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
               , opStart = oneOf $ concatMap ((: []) . head) operators
               , opLetter = oneOf $ concat operators
               , reservedOpNames = operators
               , reservedNames = ["and", "but", "then", ".",
                                  "too", "Alice", "found", "perhaps", "either",
                                  "maybe", "eventually", "because", "room",
                                  "Looking-Glass", "The", "changed", "said",
                                  "thought", "number", "sentence", "letter", "went",
                                  "through", "spoke"
                                 ]
               }

-- Generate useful parsers with makeTokenParser                 
TokenParser { identifier = p_varName
            , reservedOp = p_reservedOp
            , natural = p_natural
            , whiteSpace = p_white
            , charLiteral = p_letter
            , parens = p_parens
            , lexeme = p_lexeme
            , stringLiteral = p_stringLiteral
            } = makeTokenParser def

p_identifier = try p_arrayEl
               <|> liftM SingleElement p_varName
               <?> "identifier"
p_arrayEl = do
  var <- p_varName
  p_stringS "'s"
  pos <- p_expr
  p_stringS "piece"
  return (ArrayElement var pos)

-- Actual parser
mainparser :: String -> Parser AST
mainparser f = do
  p_white 
  sl <- manyTill p_statement p_nextfunction
  ds <- manyTill p_declaration eof
  return (AST f (((0,0), Function empty mainFunction [] MaliceInt sl) : ds))

p_separator = try (p_stringSS "too" >> (p_separatorNoSpace <|> p_separatorSpace))
              <|> p_separatorNoSpace <|> p_separatorSpace <?> "statement separator"
p_separatorNoSpace = choice $ map p_string [".", ",", "?"]
p_separatorSpace = choice $ map p_stringS ["and", "but", "then"]

-- Statement
p_statement =
  liftM2 (\p s -> ((sourceLine p, sourceColumn p), s)) getPosition (
    st <* (p_separator >> many (try p_separator)))
  where st = try p_return
             <|> try (p_varName >>= p_declare)
             <|> try (p_varName >>= p_declarearray)
             <|> try (p_identifier >>= p_incdec)
             <|> try (p_identifier >>= p_assign)
             <|> try p_print
             <|> try p_get
             <|> try p_until
             <|> try p_ifelse
             <|> try p_changercall
             <|> try (liftM FunctionCallS p_functioncall)
             <|> try (many1 (try p_comment) >> st)
             <|> (many1 p_separator >> st)

p_comment = p_stringLiteral >> p_cstring "thought Alice" >> p_separator

-- Declaration statement
p_declaration = do
  p <- getPosition
  d <- (try p_function <|> p_changer)
  return ((sourceLine p, sourceColumn p), d)

p_return = p_cstringS "Alice found" >> liftM Return p_expr

p_incdec v = choice [ p_stringSS "ate" >> return (Increase v)
                    , p_stringSS "drank" >> return (Decrease v)
                    ]

p_declare v = do
  p_cstringS "was a"
  liftM (flip Declare v) p_type

p_declarearray v = do
  p_stringS "had"
  size <- p_expr
  t <- p_type
  return (Declare (MaliceArraySize t size) v)
  
p_assign v = p_stringS "became" >> liftM (Assign v) p_expr

p_print = liftM Print (p_expr <*
                       (try (p_stringSS "spoke")
                        <|> p_cstringSS "said Alice"))

p_get = p_cstringS "what was" >> liftM Get p_identifier

-- Composite statements
p_until = do
  p_string "eventually"
  e <- p_parens p_expr
  p_stringS "because"
  liftM (Until empty e) $ manyTill p_statement $ try (p_cstringSS "enough times")

p_ifelse = do
  (p_string "perhaps" <|> p_string "either")
  e <- p_expr
  p_stringS "so"
  sl <- statements
  fmap IfElse (triplet [(empty, e, sl)])
  where
    triplet now = try (ifelse now) <|> try (elseblock now) <|> (end >> return now)
    statements = manyTill p_statement $ try (try (p_stringS "or" >> return ())
                                             <|> lookAhead end)
    end = p_cstringSS "Alice was unsure" >> optional (p_stringSS "which")
    ifelse now = do
      p_string "maybe"
      e <- p_parens p_expr <* p_stringS "so"
      sl <- statements
      triplet (now ++ [(empty, e, sl)])
    elseblock now =
      liftM ((now ++) . (: []) . (,,) empty (Int 1)) (statements <* end)
      
p_function = do
  p_white
  p_cstring "The room"
  name <- p_varName
  args <-  p_parens $ sepBy (liftM2 (flip (,)) p_type p_varName) (p_string ",")
  p_cstringS "contained a"
  ret <- p_type
  sl <- manyTill p_statement p_nextfunction
  return $ Function empty name args ret sl

p_changer = do
  p_white
  p_cstring "The Looking-Glass"
  name <- p_varName
  p_cstringS "changed a"
  t <- p_type
  sl <- manyTill (lookAhead (notFollowedBy (p_return >> return 'x')) >> p_statement) p_nextfunction
  many p_separator
  return $ Function empty name [("it", t)] t (
    sl ++ [((0,0), Return (Id (SingleElement "it")))])
  
p_nextfunction =
  eof
  <|> (try (lookAhead (p_white >> p_string "The")) >> return ())

p_changercall = do
  var <- p_identifier
  p_cstringS "went through"
  function <- p_varName
  return (Assign var (FunctionCall function [Id var]))

p_type =
  try (liftM MaliceArray (p_stringS "spider" >> p_type'))
  <|> p_type'
  where
    p_type' = liftM stringToType (p_stringSS "number"
                                  <|> p_stringSS "letter"
                                  <|> p_stringSS "sentence"
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
       <|> liftM String p_stringLiteral
       <|> liftM Id p_identifier
       <|> liftM Char p_letter
       <|> liftM Int p_int32

p_functioncall = do
  f <- p_varName
  liftM (FunctionCall f) (p_parens $ sepBy p_expr p_separator);

p_int32 = do
  int <- p_natural
  return (fromIntegral int :: Int32)
  
p_operator = choice $ map p_string operators

-- Utils
p_string = p_lexeme . string
p_stringS s = p_lexeme (string s <* p_1white)
p_stringSS s = p_lexeme (string s <* ((p_1white >> return ())
                                      <|> (lookAhead p_separatorNoSpace >> return ())))

p_cstring = p_cstringGen p_string
p_cstringS = p_cstringGen p_stringS
p_cstringSS = p_cstringGen p_stringSS
p_cstringGen p s = (mapM lexeme1 start >> p end) >> return s
  where end = last ws 
        start = init ws
        ws = words s
        lexeme1 x = p_lexeme (string x <* p_1white)

p <* q = p >>= (\x -> q >> return x)

p_1white = satisfy $ isSpace

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