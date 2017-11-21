{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..),scanTokens) where
import Expr
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn }
  $digit+                       { \s -> TokenNum (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  [\^]                          { \s -> TokenPow }
  [\/]                          { \s -> TokenDiv }
  d\/d                           { \s -> TokenDiff }
  sin                           { \s -> TokenSin }
  cos                           { \s -> TokenCos }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
{
data Token = TokenLet
           | TokenIn
           | TokenLambda
           | TokenNum Int
           | TokenSym String
           | TokenArrow
           | TokenEq
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenPow
           | TokenDiv
           | TokenDiff
           | TokenSin
           | TokenCos
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)
scanTokens = alexScanTokens
}