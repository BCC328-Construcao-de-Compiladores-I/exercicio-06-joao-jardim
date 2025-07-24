module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.List ((\\))

import L.L2.Frontend.Syntax
import Utils.Var

typeCheck :: L2 -> Either String L2
typeCheck prog = case runTcM initTcEnv (tcL2 prog) of
  ((Right (), _), _) -> Right prog 
  ((Left err, _), _) -> Left err 

-- basic monad infrastructure

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv
  = TcEnv {
      context :: [Var] -- imutable variable list
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\ env -> env{context = v : context env})

removeVar :: Var -> TcM ()
removeVar v = modify (\ env -> env {context = (context env) \\ [v]})

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m
  = runIdentity (runStateT (runWriterT (runExceptT m)) env)

-- Funções de verificação semântica

tcL2 :: L2 -> TcM ()
tcL2 (L2 ss) = mapM_ tcS2 ss 

tcS2 :: S2 -> TcM ()
tcS2 (LAssign v e) = do
  ctx <- gets context 
  when (v `elem` ctx) $ throwError $ "Assignment to immutable variable: " ++ show v 
  tcE2 e 
tcS2 (LRead _ v) = do
  ctx <- gets context 
  when (v `elem` ctx) $ throwError $ "Read into immutable variable: " ++ show v 
  -- Não verifica a expressão, pois LRead não a usa diretamente
tcS2 (LPrint e) = tcE2 e 
tcS2 (Def v e ss) = do
  tcE2 e 
  insertVar v 
  mapM_ tcS2 ss 
  removeVar v 

tcE2 :: E2 -> TcM ()
tcE2 (LVal _) = return () 
tcE2 (LVar v) = do
  ctx <- gets context 
  when (null ctx) $ return ()  -- Variáveis podem ser indefinidas fora de escopos, mas não causam erro aqui
tcE2 (LString _) = return () 
tcE2 (LAdd e1 e2) = tcE2 e1 >> tcE2 e2 
tcE2 (LMinus e1 e2) = tcE2 e1 >> tcE2 e2 
tcE2 (LMul e1 e2) = tcE2 e1 >> tcE2 e2 
tcE2 (LDiv e1 e2) = tcE2 e1 >> tcE2 e2 
tcE2 (LExpr e) = tcE2 e 