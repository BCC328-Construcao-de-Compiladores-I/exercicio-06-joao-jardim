module Utils.Value where 

import Utils.Pretty 

data Value 
  = VInt Int
  | VStr String 
    deriving (Eq, Ord, Show)

instance Pretty Value where 
  ppr (VInt n) = int n
  ppr (VStr s) = doubleQuotes (text s)

-- Arithmetic operations
(.+.) :: Value -> Value -> Either String Value
VInt n1 .+. VInt n2 = Right (VInt (n1 + n2))
VStr s1 .+. VInt n2 = Right (VStr (s1 ++ show n2)) 
VStr s1 .+. VStr s2 = Right (VStr (s1 ++ s2)) 

e1 .+. e2 = Left $ unwords ["Type error on:", pretty e1, "+", pretty e2]
(.-.) :: Value -> Value -> Either String Value
VInt n1 .-. VInt n2 = Right (VInt (n1 - n2))
e1 .-. e2 = Left $ unwords ["Type error on:", pretty e1, "-", pretty e2]

(.*.) :: Value -> Value -> Either String Value
VInt n1 .*. VInt n2 = Right (VInt (n1 * n2))
e1 .*. e2 = Left $ unwords ["Type error on:", pretty e1, "*", pretty e2]

(./.) :: Value -> Value -> Either String Value
VInt n1 ./. VInt n2
  | n2 /= 0 = Right (VInt (n1 `div` n2))
  | otherwise = Left "Division by zero"
e1 ./. e2 = Left $ unwords ["Type error on:", pretty e1, "/", pretty e2]
