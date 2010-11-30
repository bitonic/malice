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
                         
instance Show MaliceType where
  show MaliceInt = "number"
  show MaliceChar = "letter"
  show MaliceString = "sentence"
  show (MaliceArray t) = "spider " ++ show t
  show (MaliceArraySize t _) = show (MaliceArray t)
                         
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
    showDL dl

mainFunction = "_main"

type StatementList = [Statement]

showSL sl ind = init $ concatMap (\(_, s) -> ind ++ showS s (ind ++ "    ") ++ "\n") sl

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
     deriving (Eq,Show)

showS (Assign id e) _ = show id ++ " = " ++ show e
showS (Declare t n) _ = n ++ " is a " ++ show t
showS (Decrease id) _ = "Decrease " ++ show id
showS (Increase id) _ = "Increase " ++ show id
showS (Return e) _ = "Return " ++ show e
showS (Print e) _ = "Print \"" ++ show e ++ "\""
showS (Get id) _ = "Get " ++ show id
showS (Comment _) _ = "---"
showS (FunctionCallS e) _ = show e
showS (Until st e sl) ind = "Until " ++ show e ++ " becomes true {\n" ++
                            showSL sl ind ++ "\n" ++ ind ++ "\b\b\b\b}"
showS (IfElse blocks) ind = first (head blocks) ++ concatMap ifelse (tail blocks) ++
                            "\n" ++ ind ++ "\b\b\b\b}"
  where
    first (st, e, sl) = "If " ++ show e ++ " {\n" ++ showSL sl ind
    ifelse (st, e, sl) = switch ++ " {\n" ++ showSL sl ind
      where switch | e == (Int 1) = "Else"
                   | otherwise    = "Else If (" ++ show e ++ ")"


type DeclarationList = [Declaration]

showDL dl = concatMap (\(_, d) -> show d ++ "\n") dl

type DeclarationMap = Map String DeclarationAct

type Declaration = (Position, DeclarationAct)

data DeclarationAct
     -- Symbol table, function name, arguments, return type, body
     = Function SymbolTable String FunctionArgs MaliceType StatementList
     deriving (Eq)

instance Show DeclarationAct where
  show (Function st name args t sl) =
    "Function \"" ++ name ++ "\", args: " ++ showArg args ++
    ", return type: " ++ show t ++ "{\n" ++
    showSL sl "    " ++ "\n}\n"

type FunctionArgs = [(String, MaliceType)]

showArg args = "(" ++ (concat [show t ++ " " ++ n ++ ", " |
                               (n, t) <- args]) ++ ")"

data Identifier = SingleElement String -- String = name of the variable
                | ArrayElement String Expr -- Name position
                deriving (Eq)
                        
instance Show Identifier where
  show (SingleElement s) = s
  show (ArrayElement s e) = s ++ "[" ++ show e ++ "]"

data Expr
     = UnOp String Expr
     | BinOp String Expr Expr
     | FunctionCall String [Expr]
     | Int Int32
     | Char Char
     | String String
     | Id Identifier
     deriving (Eq)

instance Show Expr where
  show (UnOp op e) = "(" ++ op ++ show e ++ ")"
  show (BinOp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
  show (FunctionCall f args) = f ++ "(" ++ concatMap show args ++ ")"
  show (Int i) = show i
  show (Char c) = [c]
  show (String s) = s
  show (Id id) = show id
              
type SymbolTable = Map String (MaliceType, Int)

--Utils
stringToType "number" = MaliceInt
stringToType "letter" = MaliceChar
stringToType "sentence" = MaliceString

declName (Function _ s _ _ _) = s