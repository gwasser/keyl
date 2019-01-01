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


module Pie.Lexical.Tokens where

-- |A LexicalToken consists of a Token along with appropriate meta data.
-- |If no metadata (such as end of file reached), then meta is Nothing.
-- Implemented as a record, as suggested in a StackOverflow thread:
-- <https://stackoverflow.com/questions/34847211/getting-line-number-information-in-the-semantic-analysis-phase-using-alex-happy>
data L a = L { getPos :: AlexPosn, unPos :: a } deriving (Eq, Show)

-- |AlexPosn represents the position within the lexical stream for each token.
data AlexPosn = AlexPosn { absolute :: Int, row :: Int, col :: Int } deriving (Eq, Show)

-- |A Token of the Tiger language as read by the lexer.
-- Tokens consist of all keywords, punctuation, and data types
-- of the Tiger language, as well as special End of File (EOF) token.
-- Data type tokens have an additional constructor argument
-- representing the value of the token.
data Token = TEOF
           -- keywords
           | THE
           -- universal type
           | U
           -- atom types
           | ATOM_TYPE
           | ATOM String
           -- pair types
           | PAIR
           | CONS
           | CAR
           | CDR
           -- natural numbers
           | NAT
           | ZERO
           | ADDONE
           -- identifiers
           | VAR String
           -- numbers (integers)
           | NUM Int
           | PLUS
           -- groupings
           | LBRACE 
           | LBRACKET 
           | LPAREN 
           | RBRACE 
           | RBRACKET 
           | RPAREN
           -- interpreter commands
           | CHECKSAME
           deriving (Eq, Show)

getStr :: Token -> String
getStr (ATOM s) = s
getStr (VAR s) = s
getStr _ = error "Not a ATOM or VAR Token"

getNum :: Token -> Int
getNum (NUM n) = n
getNum _ = error "Not a NUM Token"
