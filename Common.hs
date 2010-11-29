module Common
       (
         MaliceType(..),
         Position, FileName,
         AST(..), StatementList,
         Statement, StatementAct(..),
         FunctionArgs, Identifier(..),
         Declaration, DeclarationList, DeclarationMap, DeclarationAct(..), mainFunction,
         Expr(..),
         stringToType, declName,
         SymbolTable
       ) where

import Data.Int ( Int32 )
import Data.Map ( Map )

-- Abstact Syntax Tree definition
-- The --Pos ones are used in the semantics, so that we
-- can have nice error messages.

data MaliceType = MaliceInt
                | MaliceChar
                | MaliceString
                | MaliceArray MaliceType
                | MaliceArraySize MaliceType Expr
                deriving (Show)
                         
instance Eq MaliceType where                      
  (MaliceArray t) == (MaliceArraySize t' _) = t == t'
  (MaliceArray t) == (MaliceArray t') = t == t'
  (MaliceArraySize t s) == (MaliceArraySize t' s') = t == t' && s == s'
  MaliceInt == MaliceInt = True
  MaliceChar == MaliceChar = True
  MaliceString == MaliceString = True
  _ == _ = False
                        
type Position = (Int, Int)

type FileName = String
data AST = AST FileName DeclarationList
         deriving (Eq)
                  
instance Show AST where
  show (AST fn dl) =
    "File " ++ fn ++ ":\n\n" ++
    show dl

mainFunction = "_main"

type StatementList = [Statement]

type Statement = (Position, StatementAct)

data StatementAct
     = Assign Identifier Expr
     | Declare MaliceType String
     | Decrease Identifier
     | Increase Identifier
     | Return Expr
     | Print Expr
     | Get Identifier
     | Comment String
     | FunctionCallS Expr
     -- Composite statements
     | Until SymbolTable Expr StatementList
     | IfElse [(SymbolTable, Expr, StatementList)]
     deriving (Show, Eq)

type DeclarationList = [Declaration]

showDL = map show

type DeclarationMap = Map String DeclarationAct

type Declaration = (Position, DeclarationAct)

data DeclarationAct
     -- Symbol table, function name, arguments, return type, body
     = Function SymbolTable String FunctionArgs MaliceType StatementList
     deriving (Eq, Show)

type FunctionArgs = [(String, MaliceType)]

data Identifier = SingleElement String -- String = name of the variable
                | ArrayElement String Expr -- Name position
                deriving (Eq,Show)

{-                         
instance Show Identifier where
  show (Single s) = "variable " ++ show s
  show (Array s _) = "array " ++ show s
-}

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | FunctionCall String [Expr]
     | Int Int32
     | Char Char
     | String String
     | Id Identifier
     deriving (Show, Eq)
              
type SymbolTable = Map String MaliceType

--Utils
stringToType "number" = MaliceInt
stringToType "letter" = MaliceChar
stringToType "sentence" = MaliceString

declName (Function _ s _ _ _) = s