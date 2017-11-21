{
module HappyParser where

import AlexToken
import Expr
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    let   { TokenLet }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '^'   { TokenPow }
    '/'   { TokenDiv }
    'd'   { TokenDiff }
    'sin' { TokenSin }
    'cos' { TokenCos }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%left '+' '-'
%left '*'
%%

Expr : Expr '+' Expr               { EADD $1 $3 }
     | NUM '+' Expr                { EADD (POL (PMUL (toRational $1) (PPOW "num" 0))) $3 }
     | Expr '-' Expr               { EADD $1 (EMUL (POL (PMUL (-1) (PPOW "negate" 0))) $3) }
     | Expr '*' Expr               { EMUL $1 $3 }
     | NUM '*' Expr                { EMUL (POL (PMUL (toRational $1) (PPOW "num" 0))) $3 }
     | Expr '^' NUM                { EPOW $1 (toInteger $3) }
     | Expr '/' Expr               { DIV $1 $3 }
     | 'd' VAR '(' Expr ')'        { D $2 $4 }
     | 'sin' '(' Expr ')'          { SIN $3 }
     | 'cos' '(' Expr ')'          { COS $3 }
     | '(' Expr ')'                { $2 }
     | Pol                         { POL $1 }

Pol  : Pol '+' Pol                 { PADD $1 $3 }
     | NUM '*' Pol                 { PMUL (toRational $1) $3 }
     | VAR                         { VAR $1 }
     | NUM                         { PMUL (toRational $1) (PPOW "num" 0) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> E
parseExpr = expr . scanTokens
}