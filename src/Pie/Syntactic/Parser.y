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

-- exports happyTokenParse :: [Token] -> Program
--     and happyTokenParseWithPosn :: [L Token] -> Program
--     and getAST :: Program -> Program (removes AlexPosn of all nodes)
module Pie.Syntactic.Parser (happyTokenParse, happyTokenParseWithPosn, getAST, E(..)) where 

import Control.Monad.Except

import Pie.Lexical.Tokens (Token(..), L(..), AlexPosn(..), getStr, getNum)
import Pie.Syntactic.AST (Program(..), Exp(..))
}


%name happyTokenParseWithPosn
%tokentype { L Token }
%error { parseError }

%monad { E } { thenE } { returnE }

%token 
      EOF             { L { getPos=_, unPos=TEOF } }
      
      THE             { L { getPos=_, unPos=THE } }

      U               { L { getPos=_, unPos=U } }
      
      PAIR            { L { getPos=_, unPos=PAIR } }
      CONS            { L { getPos=_, unPos=CONS } }
      CAR             { L { getPos=_, unPos=CAR } }
      CDR             { L { getPos=_, unPos=CDR } }
      
      ATOM_TYPE       { L { getPos=_, unPos=ATOM_TYPE } }
      ATOM            { L { getPos=_, unPos=ATOM s } }
      
      NAT             { L { getPos=_, unPos=NAT } }
      ZERO            { L { getPos=_, unPos=ZERO } }
      ADDONE          { L { getPos=_, unPos=ADDONE } }
  
      NUM             { L { getPos=_, unPos=NUM n } }
      '+'             { L { getPos=_, unPos=PLUS } }

      '{'             { L { getPos=_, unPos=LBRACE } }
      '}'             { L { getPos=_, unPos=RBRACE } }
      '('             { L { getPos=_, unPos=LPAREN } }
      ')'             { L { getPos=_, unPos=RPAREN } }
      '['             { L { getPos=_, unPos=LBRACKET } }
      ']'             { L { getPos=_, unPos=RBRACKET } }
      
      VAR             { L { getPos=_, unPos=VAR s } }

%%

program     : exp EOF                                       { Program $1 }

exp         : U                                             { ExpWithPosn (UniverseType) (getPos $1) }
            | '(' THE exp exp ')'                           { ExpWithPosn (TypeAnnotation $3 $4) (getPos $1) }
            | VAR                                           { ExpWithPosn (VarExp (getStr $ unPos $1)) (getPos $1) }
            | ATOM_TYPE                                     { ExpWithPosn (AtomType) (getPos $1) }
            | ATOM                                          { ExpWithPosn (AtomLiteral (getStr $ unPos $1)) (getPos $1) }
            | '(' PAIR exp exp ')'                          { ExpWithPosn (PairType $3 $4) (getPos $1) }
            | '(' CONS exp exp ')'                          { ExpWithPosn (PairCons $3 $4) (getPos $1) }
            | '(' CAR exp ')'                               { ExpWithPosn (PairCar $3) (getPos $1) }
            | '(' CDR exp ')'                               { ExpWithPosn (PairCdr $3) (getPos $1) }
            | NAT                                           { ExpWithPosn (NatType) (getPos $1) }
            | ZERO                                          { ExpWithPosn (Zero) (getPos $1) }
            | '(' ADDONE exp ')'                            { ExpWithPosn (AddOne $3) (getPos $1) }
            | NUM                                           { ExpWithPosn (NatLiteral (getNum $ unPos $1)) (getPos $1) }

     
{

-- |Convenience function used only in parser unit testing, uses fake metadata
happyTokenParse :: [Token] -> E Program
happyTokenParse ts = getAST . happyTokenParseWithPosn $ map (\t -> L {getPos=AlexPosn {absolute=1,row=1,col=1}, unPos=t}) ts

getAST :: E Program -> E Program
getAST (Failed a) = Failed a
getAST (Ok (Program p)) = returnE $ Program $ removePosn p
    where removePosn (ExpWithPosn t@(ExpWithPosn t2 pos2) pos) = removePosn t
          removePosn (ExpWithPosn (TypeAnnotation t1 t2) pos) = TypeAnnotation (removePosn t1) (removePosn t2)
          removePosn (ExpWithPosn (VarExp s) pos) = VarExp s
          removePosn (ExpWithPosn (AtomLiteral s) pos) = AtomLiteral s
          removePosn (ExpWithPosn (PairType t1 t2) pos) = PairType (removePosn t1) (removePosn t2)
          removePosn (ExpWithPosn (PairCons t1 t2) pos) = PairCons (removePosn t1) (removePosn t2)
          removePosn (ExpWithPosn (PairCar t1) pos) = PairCar (removePosn t1)
          removePosn (ExpWithPosn (PairCdr t1) pos) = PairCdr (removePosn t1)
          removePosn (ExpWithPosn (AddOne t1) pos) = AddOne (removePosn t1)
          removePosn (ExpWithPosn (NatLiteral n) pos) = NatLiteral n
          removePosn (ExpWithPosn t pos) = t

parseError :: [L Token] -> E a
parseError tokens = failE ("Parse error: (Line " ++ show errRow ++ ", Col " ++ show errCol ++ "): " ++ show nextTok)
    where errRow = row $ getPos next
          errCol = col $ getPos next
          nextTok = unPos next
          next = head tokens
          
          
-- Use a Monad to report an error
-- See example at: https://www.haskell.org/happy/doc/html/sec-monads.html

data E a = Ok a | Failed String
   deriving (Show, Eq)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e

-- Could include types representing syntax tree nodes,
-- but we already imported that above from:
-- Pie.Syntactic.Syntax

-- We also already included the lexical tokens from:
-- Pie.Lexical.Tokens

}
