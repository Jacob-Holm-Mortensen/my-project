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
                           , "\\begin{document}"
                           , "\\section{Tests from Haskell miniproject}"
                           , "\\makebox[\\textwidth][c]{\\scalebox{1.5}{"
                           , "\\begin{tabular}{| l | c | c |}"
                           , "\\hline"
                           , "\\textbf{Test case} & \\textbf{Input} & \\textbf{Result} \\\\ \\hline "
                           , contents
                           , "\\end{tabular}}}"
                           , "\\end{document}"
                           ]
  hClose handle