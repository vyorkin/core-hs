{
module Core.Ch02.Parser
  ( parseProgram'
  , parseScDefn'
  , parseExpr'

  , parseProgram
  , parseSc
  , parseExpr
  ) where

import qualified Data.Text as Text
import Core.Ch02.Language
import Core.Ch02.Lexer (Alex, Lexeme(..), Token(..), lexer, showPosn, runAlex)
}

%name parseProgram program
%name parseSc sc
%name parseExpr expr

%tokentype { Token }

%lexer { lexer } { T _ LEOF _ }

%monad { Alex }

%error { parseError }

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

%token
  'let'  { T _ LLet _ }
  'rec'  { T _ LRec _ }
  'in'   { T _ LIn _ }
  '('    { T _ LParenL _ }
  ')'    { T _ LParenR _ }
  '->'   { T _ LArrow _ }
  '.'    { T _ LDot _ }
  '>'    { T _ LGt _ }
  '<'    { T _ LLt _ }
  'case' { T _ LCase _ }
  'of'   { T _ LOf _ }
  '\\'   { T _ LLam _ }
  '+'    { T _ LPlus _ }
  '-'    { T _ LMinus _ }
  '*'    { T _ LTimes _ }
  '=='   { T _ LEq _ }
  '='    { T _ LAssign _ }
  ';'    { T _ LSemi _ }
  NAME   { T _ (LName $$) _ }
  NUM    { T _ (LNum $$) _ }

%left '=='
%left '+' '-'
%left '*' '/'

-- -----------------------------------------------------------------------------
-- Grammar rules
-- -----------------------------------------------------------------------------

%%

program :: { CoreProgram }
program : scs { $1 }

scs :: { [CoreScDefn] }
scs :        { [] }
    | scs sc { $2 : $1 }

sc :: { CoreScDefn }
sc : 'let' name names '=' expr { ($2, $3, $5) }

expr :: { CoreExpr }
expr : letrecin { $1 }
     | lam      { $1 }
     | ap       { $1 }
     | aexpr     { $1 }
     | case     { $1 }

ap :: { CoreExpr }
ap : expr aexpr { EAp $1 $2 }

case :: { CoreExpr }
case : 'case' expr 'of' alters { ECase $2 $4 }

alters :: { [CoreAlter] }
alters : alter            { [$1] }
       | alters ';' alter { $3 : $1 }

alter :: { CoreAlter }
alter : '<' NUM '>' names '->' expr { Alter $2 $4 $6 }

lam :: { CoreExpr }
lam : '\\' name names '.' expr { ELam ($2 : (reverse $3)) $5 }

letrecin :: { CoreExpr }
letrecin : 'let' rec defns
            'in' expr { ELet $2 $3 $5 }

defns :: { [CoreDefn] }
defns : defn           { [$1] }
      | defns ';' defn { $3 : $1 }

defn :: { CoreDefn }
defn : name '=' expr { Defn $1 $3 }

rec :: { Bool }
rec : 'rec' { True }
    |       { False }

names :: { [Name] }
names :            { [] }
      | names name { $2 : $1 }

name :: { Name }
name : NAME { Name $1 }

aexpr :: { CoreExpr }
aexpr : '(' expr ')' { $2 }
      | NUM  { ENum $1 }
      | name { EVar $1 }

{
-- -----------------------------------------------------------------------------
-- Module trailer
-- -----------------------------------------------------------------------------

parseError :: Token -> Alex a
parseError (T pos l raw) = error $
     "Parsing error on lexeme "
  ++ show l
  ++ " at "
  ++ showPosn pos
  ++ maybe "" (\str -> ". Input: " ++ Text.unpack str) raw

parseProgram' :: String -> Either String CoreProgram
parseProgram' = flip runAlex parseProgram

parseScDefn' :: String -> Either String CoreScDefn
parseScDefn' = flip runAlex parseSc

parseExpr' :: String -> Either String CoreExpr
parseExpr' = flip runAlex parseExpr
}
