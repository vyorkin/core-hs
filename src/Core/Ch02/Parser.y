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

-- Exported parsers
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
  '_'    { T _ LUnderscore _ }
  'let'  { T _ LLet _ }
  'rec'  { T _ LRec _ }
  'in'   { T _ LIn _ }
  '('    { T _ LParenL _ }
  ')'    { T _ LParenR _ }
  '{'    { T _ LBraceL _ }
  '}'    { T _ LBraceR _ }
  '->'   { T _ LArrow _ }
  '.'    { T _ LDot _ }
  ','    { T _ LComma _ }
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
  EOL    { T _ LEOL _ }
  NAME   { T _ (LName $$) _ }
  NUM    { T _ (LNum $$) _ }

%left 'in'
%nonassoc '<' '>'
%left '=='
%left '+' '-'
%left '*' '/'
%%

-- -----------------------------------------------------------------------------
-- Grammar rules
-- -----------------------------------------------------------------------------

program :: { CoreProgram }
program : scs { $1 }

scs :: { [CoreScDefn] }
scs :            { [] }
    | scs sep sc { $3 : $1 }

sc :: { CoreScDefn }
sc : 'let' name names '=' aexpr { ($2, $3, $5) }

expr :: { CoreExpr }
expr : ap     { $1 }
     | letin  { $1 }
     | lam    { $1 }
     | case   { $1 }
     | aexpr  { $1 }

ap :: { CoreExpr }
ap : expr aexpr { EAp $1 $2 }

case :: { CoreExpr }
case : 'case' expr 'of' alters { ECase $2 $4 }

alters :: { [CoreAlter] }
alters : alter            { [$1] }
       | alters sep alter { $3 : $1 }

alter :: { CoreAlter }
alter : tag names '->' aexpr { Alter $1 (reverse $2) $4 }

lam :: { CoreExpr }
lam : '\\' name names '.' aexpr { ELam ($2 : (reverse $3)) $5 }

letin :: { CoreExpr }
letin : 'let' rec defns 'in' aexpr { ELet $2 (reverse $3) $5 }

defns :: { [CoreDefn] }
defns : defn           { [$1] }
      | defns sep defn { $3 : $1 }

defn :: { CoreDefn }
defn : name '=' aexpr { Defn $1 $3 }

aexpr :: { CoreExpr }
aexpr : var          { $1 }
      | num          { $1 }
      | constr       { $1 }
      | '(' expr ')' { $2 }

var :: { CoreExpr }
var : name { EVar $1 }

num :: { CoreExpr }
num : NUM { ENum $1 }

constr :: { CoreExpr }
constr : '{' NUM ',' NUM '}' { EConstr $2 $4 }

tag :: { Int }
tag : '<' NUM '>' { $2 }

rec :: { Bool }
rec : 'rec' { True }
    |       { False }

names :: { [Name] }
names :            { [] }
      | names name { $2 : $1 }

name :: { Name }
name : NAME { Name $1 }

sep : EOL { $1 }
    | ';' { $1 }

opt(p) : p { Just $1 }
       |   { Nothing }

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
