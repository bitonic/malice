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
  Program <$> (sepBy p_statement (p_separator <* spaces))
  
p_separator :: CharParser () String
p_separator = choice [ string "and"
                     , string "but"
                     , string "then"
                     , string "."
                     , string ","
                     ]
              <?> "statement separator"
              
p_statement :: CharParser () Statement
p_statement = choice [ p_assign
                     , p_declare
                     , p_decinc
                     , p_return
                     ]
              <?> "statement"

------------------------------------              
-- STATEMENTS
------------------------------------

p_assign :: CharParser () Statement
p_assign = do
  v <- p_variable
  (spaces *> p_skips "became" *> (
      (spaces *> p_expression) >>= return . Assign v))
  
varChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

p_variable = many1 (oneOf varChars)

p_declare :: CharParser () Statement
p_declare = do
  v <- p_variable
  (spaces *> p_skips "was a" *>
   (string "number" >> return (Declare Int32 v)
    <|> (string "letter" >> return (Declare Char8 v))
    <?> "type"))
   
p_decinc = do
  v <- p_variable
  spaces
  (string "++" >> return (Decrease v)
   <|> (string "--" >> return (Increase v))
   <?> "operator")

p_return = do
  (p_skips "Alice found" *> (
      do e <- p_expression
         return (Return e)))
  
-----------------------------------
-- EXPRESSIONS
-----------------------------------

p_expression :: CharParser () Exp
p_expression = choice [ p_binop
                      , p_unop
                      , p_variable >>= (return . Var)
                      , many1 digit >>= (return . Int . read)
                      , between (char '\'') (char '\'') anyChar >>= (return . Char)
                      ]
               <?> "expression"
               
operators = oneOf "+-*/%^&|"

p_binop = do
  e1 <- p_expression
  spaces
  op <- operators
  spaces
  e2 <- p_expression
  spaces
  return (BinOp op e1 e2)

p_unop = do
  e1 <- p_expression
  spaces
  op <- operators
  spaces
  return (UnOp op e1)
  
p_skips :: String -> CharParser () ()
p_skips [] = return ()
p_skips s = string w *> spaces *> p_skips (unwords ws)
    where (w : ws) = words s
    