{-
    Copyright (C) 2023, Garret Wassermann.

    This file is part of keyl, the Pie language compiler,
    based on the Pie language in "The Little Typer",
    by Daniel P. Friedman and David Thrane Christiansen.

    keyl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    keyl is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with keyl. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}


{- Based on the NanoParsec parser in "Write You a Haskell" by Stephen Diehl -}
module Pie.REPL.Interpreter
    ( eval
    ) where

import Control.Applicative
import Data.Maybe
import Data.Either

import Pie.Core.AST

isNum :: PieExp -> Bool
isNum Zero = True
isNum (AddOne t) = isNum t
isNum _ = False


isVal :: PieExp -> Bool
isVal t | isNum t = True
isVal _ = False

eval' x = case x of
               AddOne t -> AddOne <$> (eval' t)
               _ -> Nothing
               
nf x = fromMaybe x (nf <$> eval' x)

eval :: PieExp -> Maybe PieExp
eval (CheckSame e1 e2) = Just $ CheckSame e1 e2
eval t = case nf t of
              nft | isVal nft -> Just nft
                  | otherwise -> Nothing -- term is "stuck"

