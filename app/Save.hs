module Save where

import System.IO

--main :: IO ()
--main = writeOutput "output.tex" tests
--main = execLaTeXT output >>= renderFile "output.tex"

writeOutput :: FilePath -> String -> IO ()
writeOutput path contents = do
  handle <- openFile path WriteMode
  mapM_ (hPutStrLn handle) [ "\\documentclass[12pt]{article}"
                           , "\\usepackage[utf8]{inputenc}"
                           , "\\usepackage{amsmath}"
                           , "\\usepackage{amsfonts}"
                           , "\\usepackage{amssymb}"
                           , "\\usepackage{graphicx}"
                           , "\\renewcommand{\\arraystretch}{1.5}"
                           , "\\begin{document}"
                           , "\\section{Tests from Haskell miniproject}"
                           , "\\makebox[\\textwidth][c]{\\scalebox{1.2}{"
                           , "\\begin{tabular}{| l | c | c |}"
                           , "\\hline"
                           , "\\textbf{Test} & \\textbf{Input} & \\textbf{Result} \\\\ \\hline "
                           , contents
                           , "\\end{tabular}}}"
                           , "\\end{document}"
                           ]
  hClose handle

makeLaTeXRow id input output = show id ++ " & $" ++ input ++ "$ & $" ++ output ++ "$ \\\\ \\hline \n"
