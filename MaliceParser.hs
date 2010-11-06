module MaliceParser
       (
         p_text
       , parseFromFile
       ) where

import ApplicativeParsec

data AST = Program [Statement]
         deriving (Show, Eq)

data MaliceType = Int32 | Char8
                deriving (Show, Eq)

data Statement
     = Assign String Exp
     | Declare MaliceType String
     | Decrease String
     | Increase String
     | Return Exp
     deriving (Show, Eq)
                        
data Exp
     = UnOp Char Exp
     | BinOp Char Exp Exp
     | Int Int
     | Char Char
     | Var String
     deriving (Show, Eq)
                 
p_text :: CharParser () AST
p_text =
  sepBy p_statement p_separator >>= return . Program
  
p_separator :: CharParser () String
p_separator = choice [ string "and"
                     , string "but"
                     , string "then"
                     , string "."
                     , string ","
                     ]
              <?> "statement separator"
              
p_statement :: CharParser () Statement
p_statement = try p_assign
          <|> try p_declare
          <|> try p_decinc
          <|> p_return
          <?> "statement"

------------------------------------              
-- STATEMENTS
------------------------------------

p_assign :: CharParser () Statement
p_assign = do
  v <- spacesAfter p_variable
  p_skips "became"
  p_expression >>= return . Assign v
  
varCharsStart = ['a'..'z'] ++ ['A'..'Z']
varChars = varCharsStart ++ ['0'..'9'] ++ "_"

p_variable
  = oneOf varCharsStart >>= (\c -> many (oneOf varChars) >>= return . ((:) c))

p_declare :: CharParser () Statement
p_declare = do
  v <- spacesAfter p_variable
  p_skips "was a"
  choice [ string "number" >> return (Declare Int32 v)
         , string "letter" >> return (Declare Char8 v)
         ]
  <?> "type"
   
p_decinc = do
  v <- spacesAfter p_variable
  choice [ string "ate" >> return (Increase v)
         , string "drank" >> return (Decrease v)
         ]
  <?> "operator"

p_return =
  p_skips "Alice found" >> (p_expression >>= return . Return)
  
-----------------------------------
-- EXPRESSIONS
-----------------------------------

p_expression :: CharParser () Exp
p_expression = try p_binop
           <|> try p_unop
           <|> (p_variable >>= (return . Var))
           <|> (many1 digit >>= (return . Int . read))
           <|> (between (char '\'') (char '\'') anyChar >>= (return . Char))
           <?> "expression"
               
operators = oneOf "+-*/%^&|"

p_binop = do
  e1 <- p_variable >>= (return . Var)
  spaces
  op <- operators
  spaces
  e2 <- p_variable >>= (return . Var)
  spaces
  return (BinOp op e1 e2)

p_unop = do
  e1 <- p_variable >>= (return . Var)
  spaces
  op <- operators
  spaces
  return (UnOp op e1)
  
p_skips :: String -> CharParser () ()
p_skips s = case ws of
                 [w'] -> string w >> spaces1 >> string w' >> spaces
                 _    -> string w >> spaces1 >> p_skips (unwords ws)
    where (w : ws) = words s
    
spaces1 = many1 space

spacesAfter p =
  p >>= (\v -> spaces1 >> return v)