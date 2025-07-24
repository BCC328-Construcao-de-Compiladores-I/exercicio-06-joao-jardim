{
module L.L2.Frontend.Parser where
import L.L2.Frontend.Lexer
import L.L2.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Debug.Trace (trace)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  id          { Token _ (TIdent $$) }
  print       { Token _ TPrint }
  read        { Token _ TRead }
  string      { Token _ (TString $$) }
  number      { Token _ (TNumber $$) }
  ','         { Token _ TComma }
  ':='        { Token _ TAssign }
  '+'         { Token _ TPlus }
  '-'         { Token _ TMinus }
  '*'         { Token _ TTimes }
  '/'         { Token _ TDiv }
  '('         { Token _ TLParen }
  ')'         { Token _ TRParen }
  ';'         { Token _ TSemicolon }
  def         { Token _ TDef }
  in          { Token _ TIn }
  end         { Token _ TEnd }

%left '(' ')'  
%left '*' '/'  
%left '+' '-'  

%%

prog
    : stmts    { L2 $1 }

stmts
    : stmt                     { [ $1 ] }
    | stmts stmt               { $1 ++ [ $2 ] }

stmt
    : assign_stmt              { $1 }
    | read_stmt                { $1 }
    | print_stmt               { $1 }
    | def_stmt                 { $1 }

assign_stmt
    : id ':=' exp ';'         { LAssign (Var $1) $3 }

read_stmt
    : read '(' string ',' id ')' ';' { LRead ($3) (Var $5) }

print_stmt
    : print '(' exp ')' ';'   { LPrint $3 }

def_stmt
    : def id ':=' exp in stmts end { Def (Var $2) $4 $6 }

exp
    : exp '+' exp             { LAdd $1 $3 }
    | exp '-' exp             { LMinus $1 $3 }
    | exp '*' exp             { LMul $1 $3 }
    | exp '/' exp             { LDiv $1 $3 }
    | '(' exp ')'             { $2 }
    | string                  { LString $1 }  -- Ajuste para usar LString se definido em Syntax.hs
    | number                  { LVal (VInt $1) }
    | id                      { LVar (Var $1) }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at token: " ++ show (map lexeme tokens)
}