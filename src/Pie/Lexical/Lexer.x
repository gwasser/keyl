{
--    Copyright (C) 2018, Garret Wassermann.
--
--    This file is part of pie, the Pie language compiler,
--    based on the Pie language in "The Little Typer",
--    by Daniel P. Friedman and David Thrane Christiansen.
--
--    pie is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    pie is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with pie. If not, see <http://www.gnu.org/licenses/>.

module Pie.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithPosn) where 

-- This implementation is partly based on the work of
-- Jyotirmoy Bhattacharya, documented in book "Alex and Happy" at
-- <https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html>.
-- The example code was originally released on GitHub
-- under terms of the GNU GPLv3 license. See:
-- <https://github.com/jmoy/alexhappy/tree/master/startcode>.

import Control.Monad.State (State, get, put, runState, evalState, execState)
import Codec.Binary.UTF8.String (encode)
import Data.Word (Word8)

import Pie.Lexical.Tokens (L(..), AlexPosn(..), Token(..))

-- This code doesn't use a wrapper, since it implements the basic
-- functions Alex requires directly at a low level
-- see <https://www.haskell.org/alex/doc/html/basic-api.html>
}

$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters

tokens :-

  <0>$white+               ;
  
  <0>the                   { plainTok THE }
  
  <0>U                     { plainTok U }
  
  <0>Atom                  { plainTok ATOM_TYPE }
  <0>\' [$alpha \-]+       { textTok (\s -> ATOM (tail s)) }
  
  <0>Nat                   { plainTok NAT }
  <0>$digit+               { textTok (\s -> NUM (read s :: Int)) }
  <0>zero                  { plainTok ZERO }
  <0>add1                  { plainTok ADDONE }
  <0>\+                    { plainTok PLUS }
  
  <0>Pair                  { plainTok PAIR }
  <0>cons                  { plainTok CONS }
  <0>car                   { plainTok CAR }
  <0>cdr                   { plainTok CDR }
  
  <0>\{                    { plainTok LBRACE }
  <0>\}                    { plainTok RBRACE }
  <0>\(                    { plainTok LPAREN }
  <0>\)                    { plainTok RPAREN }
  <0>\[                    { plainTok LBRACKET }
  <0>\]                    { plainTok RBRACKET }
  
  <0>$alpha [$alpha $digit \_]*    { textTok (\s -> VAR s) }
  
  <0,commentSC>"/*"       { beginComment }
  <commentSC>"*/"         { endComment }
  <commentSC>[.\n]        ;

{
-- happy in monad mode expects to provide lexer with a 
-- continuation for historical reasons.
-- Unneeded unless using monadic lexer mode.
-- See <https://www.haskell.org/happy/doc/html/sec-monads.html>.
--monadicLexer :: (L Token -> Lex a) -> Lex a
--monadicLexer cont = readToken >>= cont

-- |Strip away metadata from the lexer output, useful for
-- simple unit testing that doesn't need the meta.
alexMonadScanTokens :: String -> [Token]
alexMonadScanTokens str = map (unPos) $ alexMonadScanTokensWithPosn str
  
alexMonadScanTokensWithPosn :: String -> [L Token]
alexMonadScanTokensWithPosn str = tokenList (initialState str)
  where tokenList lexst = do
                 let nextTok = runState (readToken) lexst
                 case fst $ nextTok of
                         L {getPos=pos, unPos=TEOF} -> [L {getPos=pos, unPos=TEOF}]
                         t -> t : tokenList (snd nextTok)
        
  
--
-- LEXER STATES AND TYPES
--

-- actions (functions in braces above) have following type;
-- according to manual, alex doesn't care what the type of
-- these functions are, as long as they are consistently defined.
type LexAction = Int -- the state of the lexer (starts at 0)
               -> String -- processed input
               -> Lex (Maybe Token)
-- NOTE: because of currying, this effectively adds two more
-- parameters to any function that yields a LexAction,
-- as seen in many of the functions below.

-- Define a lexer state to track comments and strings
data LexerState = 
     LexerState {input :: AlexInput,
                 lexSC :: Int,       --Lexer start code
                 commentDepth :: Int,--Comment depth
                 stringBuf :: String --Temporary storage for strings
                }
     deriving Show

-- Generate initial lexer state from an input String
initialState :: String -> LexerState
initialState s = LexerState {   input = AlexInput {aiprev='\0',
                                                   aibytes=[],
                                                   airest=s,
                                                   aipos = AlexPosn {absolute=1,row=1,col=1}},
                                lexSC = 0,
                                commentDepth = 0,
                                stringBuf = ""
                                }

-- Our Lexer monad
type Lex a = State LexerState a

--
-- FUNCTIONS TO READ TOKENS
--

-- monadic wrapper for ordinary tokens
plainTok :: Token -> LexAction
plainTok t _ _ = return (Just t)

-- monadic wrapper for tokens that hold some type of input
-- (like a string or an integer), uses a function
-- to convert input String to appropriate type
textTok :: (String -> Token) -> LexAction
textTok cons _ s = return $ Just (cons s)


beginComment :: LexAction
beginComment _ _ = do
  s <- get
  put s {lexSC = commentSC,
         commentDepth = (commentDepth s)+1}
  return Nothing
  
endComment :: LexAction
endComment _ _ = do
  s <- get
  let cd = commentDepth s
  let sc' = if cd==1 then 0 else commentSC
  put s {lexSC=sc',commentDepth=cd-1}
  return Nothing


readToken :: Lex (L Token)
readToken = do
  s <- get
  case alexScan (input s) (lexSC s) of
    AlexEOF -> do
      let (AlexInput{aipos=pos}) = input s
      return L {getPos=pos, unPos=TEOF}
    AlexError inp' -> error $ "Lexical error at position " ++ (show $ aipos inp') ++ " : <<" ++ (show $ airest inp') ++ ">>"
    AlexSkip inp' _ -> do    
      put s{input = inp'}
      readToken
    AlexToken inp' n act -> do 
      let (AlexInput{airest=buf, aipos=pos}) = input s
      put s{input = inp'}
      res <- act n (take n buf)
      case res of
        Nothing -> readToken
        Just t -> return L {getPos=pos, unPos=t}

-- take input and a Lex Token, return a Token
evalLex :: String -> Lex (L a) -> (L a)
evalLex s m = evalState m (initialState s)

--
-- ALEX-REQUIRED TYPES AND FUNCTIONS
-- These functions that must be provided to Alex's basic interface
-- (see <https://www.haskell.org/alex/doc/html/basic-api.html>)
--

-- The input: last character, unused bytes, remaining string
data AlexInput = AlexInput {
    aiprev::Char,
    aibytes::[Word8],
    airest::String,
    aipos::AlexPosn} -- (abs, row, col) of position of lexer
    deriving Show

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai
  = case (aibytes ai) of
    (b:bs) -> Just (b, ai{aibytes=bs})
    [] -> case (airest ai) of
      [] -> Nothing
      (c:cs) -> let m = row $ aipos ai
                    n = col $ aipos ai
                    a = absolute $ aipos ai
                    newpos = if ((aiprev ai)=='\n')
                             then AlexPosn {absolute=a+1,row=m+1,col=1}
                             else AlexPosn {absolute=a+1,row=m,col=n+1}
                    (b:bs) = encode [c] in
                Just (b,AlexInput {aiprev=c,
                                   aibytes=bs,
                                   airest=cs,
                                   aipos=newpos})

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput {aiprev=c}) = c

}
