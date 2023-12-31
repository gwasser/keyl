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

module Pie.Core.Context where

import qualified Data.Map.Strict as Map
import Pie.Core.AST

-- A Context is a map of keys -> values,
-- where keys are variable identifiers and
-- values are the types associated with them
type Context = Map.Map String Binder

-- A Binder represents the type of variable, and can be one of 3 kinds:
-- (1) a Free binder represents a free variable, bound in a larger context
-- (2) a Def binder represents a name bound by a define expression
-- (3) a Claim binder represents a reserved claim, but a name isn't bound
data Binder = Free PieExp
            | Def PieExp PieExp -- represents type and a value
            | Claim PieExp

-- rename some common functions so don't need to always import Map
ctxInsert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
ctxInsert = Map.insert
ctxLookup :: Ord k => k -> Map.Map k a -> Maybe a
ctxLookup = Map.lookup



