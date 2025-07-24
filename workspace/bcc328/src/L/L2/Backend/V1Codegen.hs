module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import V.V1.Instr
import Utils.Value

v1Codegen :: L2 -> Code
v1Codegen (L2 ss1)
  = (concatMap s1Codegen ss1) ++ [Halt]

s1Codegen :: S2 -> Code
s1Codegen (LRead s v)
  = [Push (VStr s), Print, Input s, Store v]
s1Codegen (LPrint e1)
  = e1Codegen e1 ++ [Print]
s1Codegen (LAssign v e1)
  = e1Codegen e1 ++ [Store v]
s1Codegen (Def v e ss)
  = e1Codegen e ++
    [Store v] ++
    concatMap s1Codegen ss

e1Codegen :: E2 -> Code
e1Codegen (LVal v) = [Push v]
e1Codegen (LVar v) = [Load v]
e1Codegen (LAdd l0 l1)
  = e1Codegen l0 ++ e1Codegen l1 ++ [Add]
e1Codegen (LMinus l0 l1)
  = e1Codegen l0 ++ e1Codegen l1 ++ [Push (VInt (-1)), Mul, Add]
e1Codegen (LMul l0 l1)
  = e1Codegen l0 ++ e1Codegen l1 ++ [Mul]
e1Codegen (LDiv l0 l1)
  = error "Divisão não suportada na máquina V1. Ajuste o código ou a máquina."
e1Codegen (LString s) = [Push (VStr s)]
e1Codegen (LExpr e) = e1Codegen e