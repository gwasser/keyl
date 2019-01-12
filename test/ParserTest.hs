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

module ParserTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Pie.Parser.Combinators (runPieParser)
import Pie.Parser.AST (PieExp(..))


-- these tests are NOT necessarily valid Pie programs, but only syntactically
-- correct constructs to exercise the parser


parsecParserTests = testGroup "Parsec-based Pie parser" [testParsecAtomLiteral, testParsecNatLiteral, testParsecVarRef, testParsecAtomType, testParsecConsAtom, testParsecPairTypeAtom]
  
testParsecAtomLiteral =
  testCase "parses 'atom" $ assertEqual [] (Right $ AtomLiteral "'atom") (runPieParser "'atom")
  
testParsecAtomLiteralWS =
  testCase "parses 'atom with whitespace" $ assertEqual [] (Right $ AtomLiteral "'atom") (runPieParser "  'atom")
  
testParsecNatLiteral =
  testCase "parses 412" $ assertEqual [] (Right $ NatLiteral 412) (runPieParser "412")
  
testParsecNatLiteralWS =
  testCase "parses 412 with whitespace" $ assertEqual [] (Right $ NatLiteral 412) (runPieParser "  412")
                                                                                                                                                                                                                                                        
testParsecVarRef =
  testCase "parses xs" $ assertEqual [] (Right $ VarRef "xs") (runPieParser "xs")
  
testParsecVarRefWS =
  testCase "parses xs with whitespace" $ assertEqual [] (Right $ VarRef "xs") (runPieParser "  xs")
  
testParsecAtomType =
  testCase "parses Atom" $ assertEqual [] (Right $ AtomType) (runPieParser "Atom")
  
testParsecConsAtom =
  testCase "parses (cons 'one 'two)" $ assertEqual [] (Right $ PairCons (AtomLiteral "'one") (AtomLiteral "'two")) (runPieParser "(cons 'one 'two)")
  
testParsecPairTypeAtom =
  testCase "parses (Pair Atom Atom)" $ assertEqual [] (Right $ PairType AtomType AtomType) (runPieParser "(Pair Atom Atom)")
