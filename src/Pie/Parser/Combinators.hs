{-
    Copyright (C) 2019, Garret Wassermann.

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

module Pie.Parser.Combinators (runPieParser) where

-- Based on "Write Yourself a Scheme in 48 Hours", but using the new
-- Parsec 3 API rather than the older 2 API (Text.ParserCombinators.Parsec)
-- that is common in most tutorials such as that one.
import Text.Parsec (runParser)
import Text.Parsec.String (Parser) -- also use parseFromFile
import Text.Parsec.Char (anyChar, oneOf)
import Text.Parsec.Combinator (many1)

-- top parsing function for one expression at a time
runPieParser input = runParser symbol () "filename" input

-- symbols allowed in identifiers
symbol :: Parser Char
symbol = oneOf "'-_"
      

