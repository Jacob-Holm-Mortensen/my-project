module Lib
    ( someFunc
    ) where

someFunc :: IO Char
someFunc = getChar

-- Subject 1: Write function fib
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)