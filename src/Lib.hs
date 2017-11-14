module Lib
    ( someFunc
    ) where

someFunc :: IO String
someFunc = getLine

type K = Integer
type C = Rational
type X = String
data P = VAR X | PADD P P | PMUL C P | POW X K deriving Show
data E = POL P | EADD E E | EMUL E E | EDIV E E | D E | SIN E | COS E deriving Show

add (VAR e1) (VAR e2) = PADD (VAR e1) (VAR e2)