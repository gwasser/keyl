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


{- Based on the NanoParsec parser in "Write You a Haskell" by Stephen Diehl -}
module Pie.REPL.Pretty (
  ppexpr
) where

import Pie.Parser.AST

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty PieExp where
  ppr _ Zero = PP.text "0"
  ppr p (AddOne a) = (parensIf (p > 0) $ PP.text "add1" <+> ppr (p+1) a)
  ppr p (AtomType) = PP.text "Atom"
  ppr p (AtomLiteral s) = PP.text "'" <+> PP.text s
  ppr p (CheckSame e1 e2) = (parensIf (p > 0) $ PP.text ":check-same" <+> ppr (p+1) e1 <+> ppr (p+1) e2)

ppexpr :: PieExp -> String
ppexpr = PP.render . ppr 1 
