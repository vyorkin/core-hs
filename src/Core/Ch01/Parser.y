{
module Core.Ch01.Parser
  (
  ) where

import qualified Data.Text as Text
import Core.Ch01.Language
import Core.Ch01.Lexer (Alex, Lexeme(..), Token(..), lexer, showPosn, runAlex)
}

%name parseExpr expr

%tokentype { Token }

%lexer { lexer } { T _ LEOF _ }

%monad { Alex }

%error { parseError }

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

%token
  'let'   { T _ LLet _ }
  'rec'   { T _ LRec _ }
  'in'    { T _ LIn _ }
  '('     { T _ LParenL _ }
  ')'     { T _ LParenR _ }
  '->'    { T _ LArrow _ }
  '\\'    { T _ LLam _ }
  '='     { T _ LAssign _ }
  ';'     { T _ LSemi _ }
  NAME    { T _ (LName $$) _ }
  NUM     { T _ (LNum $$) _ }

-- -----------------------------------------------------------------------------
-- Grammar rules
-- -----------------------------------------------------------------------------

%%

expr :: { CoreExpr }
expr : letrecin { $1 }
     | lam   { $1 }
     | fact  { $1 }

lam :: { CoreExpr }
lam : '\\' name names '->' expr { ELam ($2 : (reverse $3)) $5 }

letrecin :: { CoreExpr }
letrecin : 'let' rec name '=' expr
            'in' expr { ELet $2 [($3, $5)] $7 }

rec :: { Bool }
rec : 'rec' { True }
    |       { False }

names :: { [Name] }
names :          { [] }
      | names name { $2 : $1 }

name :: { Name }
name : NAME { Name $1 }

fact :: { CoreExpr }
fact : fact atom { EAp $1 $2 }
     | atom      { $1 }

atom :: { CoreExpr }
atom : '(' expr ')' { $2 }
     | NUM   { ENum $1 }
     | name  { EVar $1 }

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

parseExpr' :: String -> Either String CoreExpr
parseExpr' = flip runAlex parseExpr
}
