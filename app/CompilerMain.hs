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

-- base modules
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseArgs

parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = version >> exit
--parseArgs []     = print $ getContents
--parseArgs fs     = print $ concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: piec [-vh] [file ..]"
version = putStr "piec - the Pie language compiler - version" >> putStrLn pieVersion
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
