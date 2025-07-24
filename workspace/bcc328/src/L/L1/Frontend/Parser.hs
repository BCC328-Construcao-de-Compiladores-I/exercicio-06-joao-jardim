-- um programa tem que aceitar ser outro programa também
-- um print só aceita coisas que estão dentro de ""
{-# LANGUAGE FlexibleInstances, TypeFamilyDependencies #-}

module L.L1.Frontend.Parser where
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import L.L1.Frontend.Syntax
import L.L1.Frontend.Lexer
import Control.Monad.Combinators.Expr
import Control.Monad.Combinators
import Data.Void (Void)
import Text.Megaparsec as T
import Text.Megaparsec.Char as TC
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Stream as S
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Debug.Trace (trace)

-- Type definitions
type Parser = Parsec Void [Lexeme]
type ParserError = ParseErrorBundle [Lexeme] Void

instance T.VisualStream [Lexeme] where
  showTokens _ ts = show ts
  tokensLength _ = length

instance T.TraversableStream [Lexeme] where
  reachOffset n posState =
    (Nothing, T.reachOffsetNoLine n posState)

debug :: String -> Parser ()
debug msg = do
  rest <- getInput
  let lastTokenMsg = case rest of
        []     -> "EOF (nenhum token restante)"
        (x:_)  -> "Próximo token: " ++ show x
  trace (msg ++ " | " ++ lastTokenMsg) (return ())


-- chainl1
chainl1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= rest
  where
    rest x = (do
                f <- op
                y <- p
                rest (f x y))
             <|> return x


-- Program parser. A program can be a statement or another program.
program :: Parser L1
program = do
    stmts <- T.many statement
    eof
    return $ L1 stmts 

-- Statement parser
statement :: Parser S1 
statement = choice 
    [ assignStmt
    , readStmt
    , printStmt
    ]

-- assign v := E;
assignStmt :: Parser S1
assignStmt = do
    TIdent var <- T.satisfy (\t -> case t of TIdent _ -> True; _ -> False)
    TAssign <- T.satisfy (== TAssign)    
    expr <- expression
    TSemicolon <- T.satisfy (== TSemicolon)
    return $ LAssign (Var var) expr

-- read(E,v); vou considerar read(string, v)
readStmt :: Parser S1
readStmt = do
  TRead <- T.satisfy (== TRead)
  TLParen <- T.satisfy (== TLParen)
  TString e <- T.satisfy (\t -> case t of TString _ -> True; _ -> False)
  TComma <- T.satisfy (== TComma)
  TIdent var <- T.satisfy (\t -> case t of TIdent _ -> True; _ -> False)
  TRParen <- T.satisfy (== TRParen)
  TSemicolon <- T.satisfy (== TSemicolon)
  return $ LRead e (Var var)

-- print(E);
printStmt :: Parser S1 
printStmt = do 
    TPrint <- T.satisfy (== TPrint)
    TLParen <- T.satisfy (== TLParen)
    -- erro aqui
    e <- expression 
    TRParen <- T.satisfy (== TRParen)
    TSemicolon <- T.satisfy (== TSemicolon)
    return $ LPrint  e 

-- Expressões
expression :: Parser E1
expression = do
    expr <- plusMinusExpr
    return expr

-- Expressões: Menor precedência: + e -
plusMinusExpr :: Parser E1
plusMinusExpr = chainl1 multDivExpr $ do
    op <- T.satisfy (\t -> case t of TPlus -> True; TMinus -> True; _ -> False)
    return $ \x y -> case op of 
        TPlus -> LAdd x y 
        TMinus -> LMinus x y

-- Expressões: Maior precedência: * e /
multDivExpr :: Parser E1
multDivExpr = chainl1 primaryExpr $ do
  op <- T.satisfy (\t -> case t of TTimes -> True; TDiv -> True; _ -> False)
  return $ \x y -> case op of 
    TTimes -> LMul x y 
    TDiv -> LDiv x y

-- Expressões: Expressões básicas
primaryExpr :: Parser E1 
primaryExpr = choice 
    [ numberExpr 
    , identExpr
    , stringExpr
    , parens
    ]

-- Expressões básicas: números
numberExpr :: Parser E1 
numberExpr = do
    TNumber n <- T.satisfy (\t -> case t of TNumber _ -> True; _ -> False)
    return $ LVal (VInt n)

-- Expr básicas: variáveis
identExpr :: Parser E1 
identExpr = do
    TIdent var <- T.satisfy (\t -> case t of TIdent _ -> True; _ -> False)
    return $ LVar (Var var)

-- Expr básicas: strings
stringExpr :: Parser E1
stringExpr = do 
    TString s <- T.satisfy (\t -> case t of TString _ -> True; _ -> False)
    return $ LString s

parens :: Parser E1 
parens = do 
    TLParen <- T.satisfy (== TLParen)
    e <- expression
    TRParen <- T.satisfy (== TRParen)
    return e 

-- Main parser function
l1Parser :: [Lexeme] -> Either String L1
l1Parser input = case parse program "L1 program" input of
  Left errBundle -> Left $ errorBundlePretty errBundle
  Right ast -> Right ast

  