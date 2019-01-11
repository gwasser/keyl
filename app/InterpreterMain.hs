{-
    Copyright (C) 2018, Garret Wassermann.

    This file is part of pie, the Pie language compiler,
    based on the Pie language in "The Little Typer",
    by Daniel P. Friedman and David Thrane Christiansen.

    pie is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pie is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pie. If not, see <http://www.gnu.org/licenses/>.
-}


module Main where

-- pie modules
import Pie.Version (pieVersion)
import Pie.Lexical.Lexer (alexMonadScanTokens)
import Pie.Syntactic.Parser (happyTokenParse, E(..))
import Pie.Syntactic.AST (Program(..))
import Pie.Parser.Combinators (runPieParser)
import Pie.REPL.Interpreter (eval)
import Pie.REPL.Pretty (ppexpr)

-- base modules
import System.Environment
import System.Exit
import System.IO

import Control.Monad.Trans
import System.Console.Haskeline
    
process :: Bool -> String -> IO ()
process ast line = do
    let res = runPieParser line --happyTokenParse $ alexMonadScanTokens line
    case res of
         Left err -> print err
         Right val -> do 
             if ast then putStr "[ast]: " >> print val else return ()
             print val
             {-case eval ex of
                  Nothing -> putStr "Error Evaluating: " >> putStrLn line
                  Just result -> print $ ppexpr result-}
                  
repl :: Bool -> IO ()
repl ast = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "pie>>> "
    case minput of
      Nothing -> outputStrLn "Goodbye. Go eat some Π!"
      Just input -> (liftIO $ process ast input) >> loop

main :: IO ()
main = getArgs >>= parseArgs

parseArgs ["-h"]         = usage   >> exit
parseArgs ["--help"]     = usage   >> exit
parseArgs ["-v"]         = version >> copyrgt >> exit
parseArgs ["--version"]  = version >> copyrgt >> exit
parseArgs ["--show-ast"] = stdintro >> repl True
parseArgs []             = stdintro >> repl False
parseArgs _              = error "Error parsing command line arguments."


stdintro = welcome >> copyrgt >> hint >> ctrld
welcome = putStr "Welcome to Pie " >> putStr pieVersion >> putStrLn " - the little dependently-typed language!"
copyrgt = putStrLn "Copyright (C) 2019, Garret Wassermann. Licensed under GNU GPLv3."
hint    = putStrLn "Enter Pie expressions for evaluation, or try :help."
usage   = putStrLn "Usage: pie [-vh]"
version = putStr "Pie interpreter " >> putStrLn pieVersion
ctrld   = putStrLn "Type CTRL+D to exit."
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
