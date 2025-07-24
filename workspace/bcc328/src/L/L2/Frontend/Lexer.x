{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer (Token (..), Lexeme (..), lexer) where 
}

%wrapper "posn"

$digit = 0-9            -- digits
$letter = [a-zA-Z]      -- letters
$white = [\ \t\n\r]     -- whitespace
@number = (\-)?$digit+  -- integers (optional minus sign)
@ident = $letter($letter | $digit | _)* -- identifiers
@string = \"[^\"]*\"    -- strings between double quotes

-- Tokens
tokens :-
  $white+               ;           -- Ignore whitespace
  "//" .*               ;           -- Ignore single-line comments
  @number               {mkNumber}  -- Numbers
  @string               {mkString}  -- Strings
  read                  {simpleToken TRead}  -- Reserved word "read"
  print                 {simpleToken TPrint} -- Reserved word "print"
  def                   {simpleToken TDef}   -- Reserved word "def" 
  in                    {simpleToken TIn}    -- Reserved word "in" 
  end                   {simpleToken TEnd}   -- Reserved word "end" 
  @ident                {mkIdent}   -- Identifiers
  ":="                  {simpleToken TAssign}  -- Assignment
  "+"                   {simpleToken TPlus}    -- Addition
  "-"                   {simpleToken TMinus}   -- Subtraction
  "*"                   {simpleToken TTimes}   -- Multiplication
  "/"                   {simpleToken TDiv}     -- Division
  "("                   {simpleToken TLParen}  -- Left parenthesis
  ")"                   {simpleToken TRParen}  -- Right parenthesis
  ";"                   {simpleToken TSemicolon} -- Semicolon
  ","                   {simpleToken TComma}   -- Comma

{
-- Data types
data Token = Token {
  pos :: (Int, Int)
  , lexeme :: Lexeme
} deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TString String
  | TIdent String
  | TRead
  | TPrint
  | TDef 
  | TIn 
  | TEnd 
  | TAssign
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TLParen
  | TRParen
  | TSemicolon
  | TComma
  | TEOF
  deriving (Eq, Ord, Show)

-- Utility functions
position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x, y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token { pos = position p, lexeme = TNumber (read s :: Int) }

mkString :: AlexPosn -> String -> Token
mkString p s = Token { pos = position p, lexeme = TString processed }
  where
    processed = case s of
      [] -> []
      [_] -> []
      _ -> init (drop 1 s)

mkIdent :: AlexPosn -> String -> Token
mkIdent p s = Token { pos = position p, lexeme = TIdent s }

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token { pos = position p, lexeme = lx }

lexer :: String -> [Token]
lexer = alexScanTokens
}