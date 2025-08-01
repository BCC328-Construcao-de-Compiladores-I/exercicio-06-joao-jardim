import L.L2.Frontend.Lexer
import L.L2.Interpreter.Interp
import L.L2.Frontend.Syntax
import L.L2.Frontend.Parser
import L.L2.Frontend.TypeCheck
import L.L2.Backend.V1Codegen
import L.L2.Backend.CCodegen
import Utils.Pretty

import Data.Map (Map)
import qualified Data.Map as Map 

import System.Environment
import System.FilePath
import System.IO (writeFile)
import System.Process
import System.Exit -- Adicionado para importar ExitSuccess e ExitFailure

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [V1Compile file] ->
    v1Compiler file
  [CCompile file] ->
    cCompiler file
  _ -> helpMessage

-- Implement the function to do lexical analysis for L2 programs and outputs the tokens

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
      TDef          -> "Palavra reservada def Linha:" ++ show line ++ " Coluna:" ++ show col
      TIn           -> "Palavra reservada in Linha:" ++ show line ++ " Coluna:" ++ show col
      TEnd          -> "Palavra reservada end Linha:" ++ show line ++ " Coluna:" ++ show col
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

-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = do
  content <- readFile file
  putStrLn "Executando analisador léxico...\n"
  let tokens = lexer content
  putStrLn "Analisador léxico executado.\n"
  putStrLn "Executando LALR...\n"
  let asts = parse tokens 
  putStrLn "LALR executado!\n"
  print asts

-- Implement the whole interpreter pipeline: lexical, syntax, and semantic analysis, then interpret the program

interpret :: FilePath -> IO ()
interpret file = do
  content <- readFile file
  let tokens = lexer content
  let asts = parse tokens 
  putStrLn "Analisador léxico e sintático executado!\n"
  case typeCheck asts of
    Left err -> putStrLn $ "Erro de análise semântica: " ++ err
    Right checkedAst -> do
      putStrLn "\nExecutando interpretador L1...\n"
      result <- evalL2 checkedAst
      case result of
        Left err -> putStrLn $ "Erro de interpretação: " ++ err
        Right env -> do
          putStrLn "Programa executado com sucesso!\n"
          putStrLn "Ambiente final:"
          case env of
            (e:_) -> mapM_ (\(var, val) -> putStrLn $ pretty var ++ " = " ++ pretty val) (Map.toList e)
            [] -> putStrLn "Ambiente vazio" 
          putStrLn "Interpretation completed."

-- Implement the V1 compiler pipeline: lexical, syntax, and semantic analysis, then generate V1 code

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  content <- readFile file
  let tokens = lexer content
  putStrLn "Analisador léxico executado.\n"
  let asts = parse tokens 
  putStrLn "Analisador sintático executado.\n"
  case typeCheck asts of
    Left err -> putStrLn $ "Erro de análise semântica: " ++ err
    Right checkedAst -> do
      putStrLn "Análise semântica executada com sucesso.\n"
      let v1Code = v1Codegen checkedAst
      let outputFile = replaceExtension file "v1"
      writeFile outputFile (pretty v1Code)
      putStrLn $ "Código V1 gerado com sucesso em: " ++ outputFile

-- Implement the C compiler pipeline: lexical, syntax, and semantic analysis, then generate C code, compile with GCC, and execute

cCompiler :: FilePath -> IO ()
cCompiler file = do
  content <- readFile file
  let tokens = lexer content
  putStrLn "Analisador léxico executado.\n"
  let asts = parse tokens 
  putStrLn "Analisador sintático executado.\n"
  case typeCheck asts of
    Left err -> putStrLn $ "Erro de análise semântica: " ++ err
    Right checkedAst -> do
      putStrLn "Análise semântica executada com sucesso.\n"
      let cCode = cL2Codegen checkedAst
      let cFile = replaceExtension file "c"
      writeFile cFile cCode
      putStrLn $ "Código C gerado com sucesso em: " ++ cFile
      let execFile = replaceExtension file ""
      let gccCmd = "gcc -o " ++ execFile ++ " " ++ cFile
      compileExitCode <- system gccCmd
      case compileExitCode of
        ExitSuccess -> do
          putStrLn $ "Executável gerado com sucesso em: " ++ execFile
          let runCmd = "./" ++ execFile
          runExitCode <- system runCmd
          case runExitCode of
            ExitSuccess -> putStrLn "Execução concluída com sucesso."
            ExitFailure code -> putStrLn $ "Erro na execução do programa. Código de saída: " ++ show code
        ExitFailure code -> putStrLn $ "Erro na compilação com GCC. Código de saída: " ++ show code

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L2 language"
                       , "Usage: l2 [--lexer-only | --parse-only | --interpret | --v1-compile | --c-compile | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--interpret: does the syntax analysis and interpret the input program."
                       , "--v1-compile: compiles the L2 program to V1 machine code."
                       , "--c-compile: compiles the L2 program to C code, generates an executable with GCC, and executes it."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | V1Compile FilePath
  | CCompile FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1" : arg : _) -> [V1Compile arg]
    ("--c" : arg : _) -> [CCompile arg]
    _ -> [Help]