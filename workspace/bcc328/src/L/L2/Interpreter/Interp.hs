module L.L2.Interpreter.Interp where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Maybe (fromMaybe)

import L.L2.Frontend.Syntax
import Utils.Pretty
import Utils.Value 
import Utils.Var 
import System.IO (hFlush, stdout)

type Env = [Map Var Value] -- Lista de ambientes para suportar escopos aninhados

evalL2 :: L2 -> IO (Either String Env)
evalL2 (L2 ss)
  = foldM step (Right [Map.empty]) ss
  where 
    step ac@(Left _) _ = pure ac 
    step (Right env) s2 = evalS2 env s2

evalS2 :: Env -> S2 -> IO (Either String Env)
evalS2 env (LRead s v)
  = do 
      putStr s 
      hFlush stdout
      val <- readValue
      pure (Right $ updateEnv v val env)
evalS2 env (LPrint e)
  = case evalE2 env e of
      Left err -> pure $ Left err 
      Right val -> do 
        putStrLn (pretty val)
        pure (Right env) 
evalS2 env (LAssign v e)
  = case evalE2 env e of
      Left err -> pure $ Left err 
      Right val -> pure (Right $ updateEnv v val env)
evalS2 env (Def v e ss)
  = case evalE2 env e of
      Left err -> pure $ Left err 
      Right val -> do
        let newEnv = Map.insert v val Map.empty -- Novo escopo com a definição de v
        result <- foldM step (Right (newEnv : env)) ss -- Empilha o novo escopo e avalia o block
        case result of
          Left err -> pure $ Left err
          Right env' -> case env' of
            (_:rest) -> pure (Right rest)  -- Desempilha o escopo interno ao sair, com verificação
            [] -> pure $ Left "Internal error: empty environment stack" 
        where
          step ac@(Left _) _ = pure ac 
          step (Right env') s = evalS2 env' s

readValue :: IO Value 
readValue = (VInt . read) <$> getLine 

evalE2 :: Env -> E2 -> Either String Value
evalE2 _ (LVal v) = Right v 
evalE2 env (LVar v) 
  = case lookupVar v env of
      Just val -> Right val 
      Nothing -> Left ("Undefined variable: " ++ pretty v)
evalE2 env (LAdd l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2 
      v1 .+. v2
evalE2 env (LMul l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2
      v1 .*. v2
evalE2 env (LMinus l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2 
      v1 .-. v2
evalE2 env (LDiv l1 l2) 
  = do 
      v1 <- evalE2 env l1
      v2 <- evalE2 env l2 
      v1 ./. v2
evalE2 _ (LString s) 
  = Right (VStr s)
evalE2 env (LExpr e) 
  = evalE2 env e

-- Função auxiliar para atualizar o ambiente
updateEnv :: Var -> Value -> Env -> Env
updateEnv v val env
  = case env of
      (e:es) -> case es of
        [] -> Map.insert v val e : es  -- Escopo global: permite reatribuição
        _  -> case Map.member v e of
          True -> error "Attempt to reassign immutable variable" : es 
          False -> Map.insert v val e : es 
      [] -> error "Internal error: empty environment stack" : env 

-- Função auxiliar para procurar variável em escopos aninhados
lookupVar :: Var -> Env -> Maybe Value
lookupVar v [] = Nothing
lookupVar v (e:es) = case Map.lookup v e of
  Just val -> Just val
  Nothing -> lookupVar v es