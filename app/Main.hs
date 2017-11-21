module Main where

import qualified HappyParser
import Eval
import Expr
import Save
import Data.Typeable

runEvalWith :: (String -> E) -> String -> IO ()
runEvalWith parseExpr input = do
                              let internalData = parseExpr input
                              putStrLn $ "\nSource string:\n" ++ source internalData
                              putStrLn $ "\nInternal data:\n" ++ show internalData ++ "\n"
                              startReducing internalData
                              putStrLn $ "Converted to standard math:\n" ++ source (fullyReduce internalData)

main :: IO ()
main = do
       putStrLn "\n"
       run True 1 ""

run p id content = do
                   if p
                   then putStrLn "Hello, please input an expression to reduce:"
                   else putStrLn "\nWaiting for another expression to reduce:"
                   input <- getLine
                   if input /= ":quit" && input /= ":q"
                   then do
                        runEvalWith HappyParser.parseExpr input
                        run False (id + 1) (content ++ show id ++ " & " ++ "2" ++ " & " ++ "3" ++ " \\\\ \\hline \n")
                   else do
                        putStrLn "Saving session and exiting"
                        let content = "1" ++ " & " ++ "2" ++ " & " ++ "3" ++ " \\\\ \\hline \n"
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
