import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer 
import L.L1.Frontend.Parser
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr
import L.L1.Frontend.LALR
import Data.Map (Map)
import qualified Data.Map as Map 

import System.Environment
import System.FilePath
import System.Process 

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts 

-- Running the compiler / interpreter 
runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of 
  [Lexer file] ->
    lexerOnly file
  [ADRParser file] ->
    recursiveParser file
  [Lalr file] ->
    lalrParser file
  [IntL1 file] ->
    interpreterL1 file
  _ -> helpMessage

-- Implement the function to do lexical analysis for L1 programs
lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  let tokens = lexer content
  mapM_ print tokens
  where
    printToken (Token { pos = (line, col), lexeme = lex }) = putStrLn $ case lex of
      TIdent s      -> "Identificador " ++ s ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
      TNumber n     -> "Número " ++ show n ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
      TString s     -> "String \"" ++ s ++ "\" Linha:" ++ show line ++ " Coluna:" ++ show col
      TRead         -> "Palavra reservada read Linha:" ++ show line ++ " Coluna:" ++ show col
      TPrint        -> "Palavra reservada print Linha:" ++ show line ++ " Coluna:" ++ show col
      TAssign       -> "Atribuição := Linha:" ++ show line ++ " Coluna:" ++ show col
      TPlus         -> "Operador + Linha:" ++ show line ++ " Coluna:" ++ show col
      TMinus        -> "Operador - Linha:" ++ show line ++ " Coluna:" ++ show col
      TTimes        -> "Operador * Linha:" ++ show line ++ " Coluna:" ++ show col
      TDiv          -> "Operador / Linha:" ++ show line ++ " Coluna:" ++ show col
      TLParen       -> "Parêntesis ( Linha:" ++ show line ++ " Coluna:" ++ show col
      TRParen       -> "Parêntesis ) Linha:" ++ show line ++ " Coluna:" ++ show col
      TSemicolon    -> "Ponto e vírgula ; Linha:" ++ show line ++ " Coluna:" ++ show col
      TComma        -> "Vírgula , Linha:" ++ show line ++ " Coluna:" ++ show col
      TEOF          -> "" -- Ignore EOF token in output

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  --content vai receber a lista de Tokens pelo analisador léxico
  content <- readFile file
  let tokens = map lexeme (lexer content)
  mapM_ print tokens
  case l1Parser tokens of
    Left err -> putStrLn $ "Erro de parsing: " ++ err
    Right ast -> putStrLn $ "\n\nÁrvore de sintaxe:\n" ++ show ast  -- Usando show

lalrParser :: FilePath -> IO ()
lalrParser file = do
  -- primeiro faz a analise lexica
  content <- readFile file
  putStrLn "Executando analisador léxico...\n"
  let tokens = lexer content
  putStrLn "Analisador léxico executado.\n"
  putStrLn "Executando LALR...\n"
  let asts = parse tokens 
  putStrLn "LALR executado!\n"
  print asts

interpreterL1 :: FilePath -> IO ()
interpreterL1 file = do
  content <- readFile file
  let tokens = lexer content
  let asts = parse tokens 
  putStrLn "Analisador léxico e sintático executado!\n"
  putStrLn "\nExecutando interpretador L1...\n"
  result <- evalL1 asts
  case result of
    Left err -> putStrLn $ "Erro de interpretação: " ++ err
    Right env -> do
      putStrLn "Programa executado com sucesso!\n"
      putStrLn "Ambiente final:"
      mapM_ (\(var, val) -> putStrLn $ pretty var ++ " = " ++ pretty val) (Map.toList env)

helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--interpreterL1 |--recursive | --lalr |  --lexer-only | --help]"
                       , "--interpreterL1: performs the interpreter using lalr parsing."                     
                       , "--recursive: performs recursive descent parsing of the input file."
                       , "--lalr: performs LALR parsing of the input file."
                       , "--lexer-only: performs lexical analysis of the input file."
                       , "--help: prints this help message"
                       ]

-- Parse command line arguments 
data Option 
  = Help 
  | Lexer FilePath
  | ADRParser FilePath
  | Lalr FilePath
  | IntL1 FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [ADRParser arg]
    ("--lalr" : arg : _) -> [Lalr arg]
    ("--interpreterL1" : arg : _) -> [IntL1 arg]
    _ -> [Help]