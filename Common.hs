module Common
       (
         MaliceType(..),
         Position, FileName,
         AST(..), StatementList,
         Statement, StatementAct(..),
         FunctionArgs, Identifier(..),
         Expr(..),
         stringToType,
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
                deriving (Show, Eq)
                        
type Position = (Int, Int)

type FileName = String
data AST = AST FileName SymbolTable StatementList
         deriving (Show, Eq)

type StatementList = [Statement]

type Statement = (Position, StatementAct)

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
     | FunctionCall Expr
     -- Composite statements
     | Until SymbolTable Expr StatementList
     | IfElse [(SymbolTable, Expr, StatementList)]
     | Function SymbolTable String FunctionArgs MaliceType StatementList
     | Changer SymbolTable String MaliceType StatementList
     deriving (Show, Eq)

type FunctionArgs = [(String, MaliceType)]

data Identifier = Single String
                | Array String Expr -- Name position
                deriving (Show, Eq)

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | FunctionOp String [Expr]
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