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

module Pie.Core.AST where

import Data.Text as T
import Data.Text (Text)

-- a Pie program is simply an expression
data Program = Program PieExp
                deriving (Show, Eq)

-- page 392 of "The Little Typer" provides a simple grammar
data PieExp  
      = TypeAnnotation PieExp PieExp
      | VarRef Text
      | AtomType
      | AtomLiteral Text
      | PairType PieExp PieExp
      | PairCons PieExp PieExp
      | PairCar PieExp
      | PairCdr PieExp
      | NatType
      | Zero
      | AddOne PieExp
      | NatLiteral Int
      | UniverseType
      -- interpreter commands (judgments)
      | TypeOrExpr PieExp
      | CheckSame PieExp PieExp
      | Norm PieExp
      | NormType PieExp
      | Rep PieExp
      deriving (Show, Eq)

