{
module L.L1.Frontend.LALR where
import L.L1.Frontend.Lexer
import L.L1.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Debug.Trace (trace)

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  --null        { TokenKeyword "null" }
  id          { Token _ (TIdent $$) }
  print       { Token _ TPrint}
  read        { Token _ TRead}
  string      { Token _ (TString $$) }
  number      { Token _ (TNumber $$) }
  ','         { Token _ TComma }
  ':='        { Token _ TAssign }
  '+'         { Token _ TPlus }
  '-'         { Token _ TMinus  }
  '*'         { Token _ TTimes  }
  '/'         { Token _ TDiv  }
  '('         { Token _ TLParen }
  ')'         { Token _ TRParen }
  ';'         { Token _ TSemicolon }
 
%left '(' ')'  
%left '*' '/'  
%left '+' '-'  

%%

prog
    : defs    { L1 $1 }

defs
    : def                      { [ $1 ] }
    | defs def                 { $1 ++ [ $2 ] }

def
    : assign_def              { $1 }
    | read_def                { $1 }
    | print_def               { $1 }

assign_def
    : id ':=' exp ';'           { LAssign (Var $1) $3 }

--aqui passo exp. Talvez tenha que passar string
read_def
    : read '(' string ',' id ')' ';'          { LRead ($3) (Var $5)}

print_def
    : print '(' exp ')' ';'    { LPrint $3}  -- Corrigido para exigir ()

-- exps
--   : exp                      { $1 }
--   | exps ',' exp             { $1 ++ [ $3 ] }

exp
    : exp '+' exp              { LAdd $1 $3 }
    | exp '-' exp              { LMinus $1 $3 }
    | exp '*' exp              { LMul $1 $3 }
    | exp '/' exp              { LDiv $1 $3 }
    | '(' exp ')'              { $2 }  -- Permite parênteses
    | string                   { LString $1 }
    | number                   { LVal (VInt $1) }
    | id                       { LVar (Var $1) }
{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at token: " ++ show (map lexeme tokens)
}