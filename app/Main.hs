module Main where

import qualified HappyParser
import Eval
import Expr
import Save
import Data.Typeable

runEvalWith parseExpr input = do
                              let internalData = parseExpr input
                              putStrLn $ "\nSource string:\n" ++ source internalData
                              putStrLn $ "\nInternal data:\n" ++ show internalData ++ "\n"
                              startReducing internalData
                              let output = fullyReduce internalData
                              putStrLn $ "Converted to standard math:\n" ++ source output
                              return output

main :: IO ()
main = do
       putStrLn "\n"
       run True 1 ""

run p id content = do
                   if p
                   then do
                        putStrLn " -- use d/dx() to differentiate an expression by the variable x):"
                        putStrLn " -- use ^k to set expressions to the power of k (integer)):"
                        putStrLn " -- use sin() to take sinus):"
                        putStrLn " -- use cos() to take cosinus):"
                        putStrLn " -- use + to add):"
                        putStrLn " -- use * to multiply):"
                        putStrLn " -- use / to divide):"
                        putStrLn " -- write \":q\" or \":quit\" to save and end session):"
                        putStrLn "Hello, please input an expression to reduce :"
                   else putStrLn "\nWaiting for another expression to reduce:"
                   input <- getLine
                   if input /= ":quit" && input /= ":q"
                   then do
                        output <- runEvalWith HappyParser.parseExpr input
                        let src = HappyParser.parseExpr input
                        let rows = content ++ makeLaTeXRow id (sourceLaTeX src) (sourceLaTeX output)
                        run False (id + 1) rows
                   else do
                        putStrLn "Saving session and exiting"
                        writeOutput "output.tex" content

-- -- initiate
-- Function to iteratively reduce if more reductions can be applied
startReducing e = let reduced = reduce e
             in if e == reduced
                then do
                     putStrLn "No further reductions can be applied. Resulting expression:"
                     putStrLn (removeExcessChars (show e ++ "\n"))
                else do
                     putStrLn "Applying reductions:"
                     putStrLn (removeExcessChars (show reduced))
                     startReducing reduced

removeExcessChars sentence = let repl '\\' = ' '
                                 repl c = c
                             in map repl sentence

fullyReduce e = let reduced = reduce e
                in if e == reduced
                then e
                else fullyReduce reduced
