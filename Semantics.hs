module Semantics where

import Parser

maliceSemantics :: Program -> IO Bool
maliceSemantics (Program sl)
  = maliceSemanticsSL sl []
    
checkDeclaration :: String -> [String] -> IO Bool -> IO Bool
checkDeclaration var vars f
  = do if elem var vars
         then f
         else do putStrLn ("Semantics error: Variable " ++ var ++ " is used before it is declared.")
                 return False
                 
maliceSemanticsSL :: StatementList -> [String] -> IO Bool
maliceSemanticsSL (Assign var e : sl) vars
  = checkDeclaration var vars (do eSem <- maliceSemanticsExp e vars
                                  if eSem
                                    then maliceSemanticsSL sl vars
                                    else return False)
maliceSemanticsSL (Declare var : sl) vars                 
  = do if elem var vars
         then do putStrLn ("Semantics error: Variable " ++ var ++ " declared twice.")
                 return False
         else maliceSemanticsSL sl (var : vars)
maliceSemanticsSL (Decrease var : sl) vars
  = checkDeclaration var vars (maliceSemanticsSL sl vars)
maliceSemanticsSL (Increase var : sl) vars
  = checkDeclaration var vars (maliceSemanticsSL sl vars)
maliceSemanticsSL [Return e] vars              
  = maliceSemanticsExp e vars
    
maliceSemanticsExp :: Exp -> [String] -> IO Bool    
maliceSemanticsExp (UnOp var e) vars
  = checkDeclaration var vars (maliceSemanticsExp e vars)
maliceSemanticsExp (BinOp var e1 e2) vars    
  = checkDeclaration var vars (do maliceSemanticsExp e1 vars
                                  maliceSemanticsExp e2 vars)
maliceSemanticsExp _ _    
  = return True