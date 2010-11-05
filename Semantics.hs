module Semantics where

import Parser
import Data.Map as Map
import qualified Data.Map as M

type SymbTable = Map String MaliceType
type ErrorMessage = String

maliceSemantics :: Program -> (Maybe SymbTable, String)
maliceSemantics (Program sl)    
  = case last sl of
         (Return _) -> maliceSemanticsSL sl M.empty
         _          -> (Nothing, "The program does not end with" ++
                                 " a return statement."
                                 
maliceSemanticsSL :: StatementList -> SymbTable -> (Maybe SymbTable, String)
maliceSemanticsSL [Return _] t    
  = (Just t, "")
maliceSemanticsSL (Assign v e : sl) t    
  | eTable == Nothing   = (eTable, eType, eError)
  | checkType v eType t = maliceSemanticsSL sl t
  | otherwise           = (Nothing, "Type error: variable"
  where
    (eTable, eType, eError) = maliceSemanticsExp e t
      