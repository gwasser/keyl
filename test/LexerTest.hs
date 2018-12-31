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

module LexerTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Pie.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithPosn)
import Pie.Lexical.Tokens (Token(..), L(..), AlexPosn(..))


tokenizerTests = testGroup "Alex-based Pie lexer" [testLexerATOMTYPE, testLexerATOMATOM, testLexerATOMVAR, testLexerUTYPE, testLexerUVAR, testLexerUNIT, testLexerCONS]

testLexerATOMTYPE =
  testCase "accepts input 'Atom' as ATOM_TYPE" $ assertEqual [] ([ATOM_TYPE, TEOF]) (alexMonadScanTokens "Atom")
  
testLexerATOMATOM =
  testCase "accepts input ''atom' as ATOM" $ assertEqual [] ([ATOM "atom", TEOF]) (alexMonadScanTokens "'atom")
  
testLexerATOMVAR =
  testCase "accepts input 'atom' as VAR" $ assertEqual [] ([VAR "atom", TEOF]) (alexMonadScanTokens "atom")
  
testLexerUTYPE =
  testCase "accepts input 'U' as U" $ assertEqual [] ([U, TEOF]) (alexMonadScanTokens "U")
  
testLexerUVAR =
  testCase "accepts input 'u' as VAR" $ assertEqual [] ([VAR "u", TEOF]) (alexMonadScanTokens "u")
  
testLexerUNIT =
  testCase "accepts input 'Unit' as VAR" $ assertEqual [] ([VAR "Unit", TEOF]) (alexMonadScanTokens "Unit")
  
testLexerCONS =
  testCase "accepts input '(cons 'one 'two)' as CONS" $ assertEqual [] ([LPAREN, CONS, ATOM "one", ATOM "two", RPAREN, TEOF]) (alexMonadScanTokens "(cons 'one 'two)")


                                                                                                                                                                                                                            
