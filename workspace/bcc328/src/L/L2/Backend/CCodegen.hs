module L.L2.Backend.CCodegen where

import L.L2.Frontend.Syntax
import Utils.Pretty
import Utils.Value

-- Função auxiliar para indentação
indent :: Int -> [String] -> [String]
indent n lines = map (\line -> replicate n ' ' ++ line) lines

-- Função de nível superior para geração de código C a partir de L2
cL2Codegen :: L2 -> String
cL2Codegen (L2 ss) = unlines $
  [ "#include <stdio.h>"
  , "// code generated for L2 expressions"
  , "int main () {"
  ] ++
  concatMap (indent 4) (map generateStmt ss) ++
  [ "    return 0;"
  , "}"
  ]
  where
    indent n lines = map (\line -> replicate n ' ' ++ line) lines

-- Gera código C para um comando S2
generateStmt :: S2 -> [String]
generateStmt (LAssign v e) = ["int " ++ pretty v ++ " = " ++ generateExp e ++ ";"]

generateStmt (LRead s v) = [ "printf(\"" ++ s ++ "\\n\");"
                           , "scanf(\"%d\", &" ++ pretty v ++ ");"
                           ]
  
generateStmt (LPrint e) = 
  case e of 
    LString s -> ["printf(\"%s\\n\", " ++ generateExp e ++ ");"]
    LAdd (LString s) e2 -> ["printf(\"%s\", " ++ "\"" ++ s ++ "\"" ++ ");"
                           , "printf(\"%d\\n\", " ++ generateExp e2 ++ ");"]
    LAdd e1 (LString s) -> ["printf(\"%d\", " ++ generateExp e1 ++ ");"
                            , "printf(\"%s\\n\", " ++ "\"" ++ s ++ "\"" ++ ");"]
    _         -> ["printf(\"%d\\n\", " ++ generateExp e ++ ");"]
generateStmt (Def v e ss) = ["{"]
                          ++ indent 4 ["int " ++ pretty v ++ " = " ++ generateExp e ++ ";"] 
                          ++ concatMap (indent 4 . generateStmt) ss
                          ++ ["}"] 

-- Gera código C para uma expressão E2
generateExp :: E2 -> String
generateExp (LVal (VInt n)) = show n
generateExp (LVar v) = pretty v
generateExp (LString s) = "\"" ++ s ++ "\"" -- Simplificação, pois C requer tipo compatível (pode ser ajustado para char* se necessário)
generateExp (LAdd e1 e2) = "(" ++ generateExp e1 ++ " + " ++ generateExp e2 ++ ")"
generateExp (LMinus e1 e2) = "(" ++ generateExp e1 ++ " - " ++ generateExp e2 ++ ")"
generateExp (LMul e1 e2) = "(" ++ generateExp e1 ++ " * " ++ generateExp e2 ++ ")"
generateExp (LDiv e1 e2) = "(" ++ generateExp e1 ++ " / " ++ generateExp e2 ++ ")"
generateExp (LExpr e) = generateExp e