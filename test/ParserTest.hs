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

import Pie.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithPosn)
import Pie.Lexical.Tokens (Token(..), L(..), AlexPosn(..))
import Pie.Syntactic.Parser (E(..), happyTokenParse, happyTokenParseWithPosn)
import Pie.Syntactic.AST (Program(..), Exp(..))

parse = happyTokenParse . alexMonadScanTokens

parseWithPosn = happyTokenParseWithPosn . alexMonadScanTokensWithPosn

program = Ok . Program

-- these tests are NOT necessarily valid Pie programs, but only syntactically
-- correct constructs to exercise the parser
parserTests = testGroup "Happy-based Pie parser" [testParserAtomTypeWithPosn, testParserAtomType, testParserAtomAtom, testParserPairAtomNat, testParserConsAtoms, testParserCarCdrCons, testParserAddOneZero, testParserAddOne42, testParserTheAtomIsU]

testParserAtomTypeWithPosn =
  testCase "parses 'Atom' with position data" $ assertEqual [] (program (ExpWithPosn AtomType (AlexPosn { row=1, col=1, absolute=1 }))) (parseWithPosn "Atom")
  
testParserAtomType =
  testCase "parses 'Atom'" $ assertEqual [] (program AtomType) (parse "Atom")
  
testParserAtomAtom =
  testCase "parses ''atom'" $ assertEqual [] (program (AtomLiteral "atom")) (parse "'atom")
  
testParserPairAtomNat =
  testCase "parses '(Pair Atom Nat)'" $ assertEqual [] (program (PairType AtomType NatType)) (parse "(Pair Atom Nat)")
  
testParserConsAtoms =
  testCase "parses '(cons 'alice 'bob)'" $ assertEqual [] (program (PairCons (AtomLiteral "alice") (AtomLiteral "bob"))) (parse "(cons 'alice 'bob)")

testParserCarCdrCons =
  testCase "parses '(cons (car (cons 'alice 'bob)) (cdr x))'" $ assertEqual [] (program (PairCons (PairCar (PairCons (AtomLiteral "alice") (AtomLiteral "bob"))) (PairCdr $ VarExp "x"))) (parse "(cons (car (cons 'alice 'bob)) (cdr x))")

testParserAddOneZero =
  testCase "parses '(add1 zero)'" $ assertEqual [] (program (AddOne Zero)) (parse "(add1 zero)")
  
testParserAddOne42 =
  testCase "parses '(add1 42)'" $ assertEqual [] (program (AddOne (NatLiteral 42))) (parse "(add1 42)")
  
testParserTheAtomIsU =
  testCase "parses '(the 'some-atom U)'" $ assertEqual [] (program (TypeAnnotation (AtomLiteral "some-atom") (UniverseType))) (parse "(the 'some-atom U)")
                                                                                                                                                                                                                                                        
                             
