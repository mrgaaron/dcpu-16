module Assembler where

import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token

data Binop =      SET 
                | ADD 
                | SUB 
                | MUL 
                | DIV 
                | MOD 
                | SHL 
                | SHR 
                | AND 
                | BOR 
                | XOR 
                | IFE 
                | IFN 
                | IFG 
                | IFB  
                    deriving Show

data Expr =   Register String
            | MemLocation Integer
            | Bin Binop Expr Expr
                    deriving Show

assemblerStyle  :: LanguageDef st
assemblerStyle = LanguageDef
		{ commentStart	 = ";"
		, commentEnd	 = "\n"
                , commentLine    = ";"
		, nestedComments = False
                , identStart     = char '['
                , identLetter    = char ']'
                , opStart        = alphaNum
                , opLetter       = char ':'
		, reservedNames  = ["A", "B", "C", "X", "Y", "Z", "I", "J"]
		, reservedOpNames= ["SET", "ADD", "SUB", "MUL", "DIV", "MOD",
                                    "SHL", "SHR", "AND", "BOR", "XOR", "IFE",
                                    "IFN", "IFG", "IFB"]
                , caseSensitive  = False
		}

lexer = makeTokenParser assemblerStyle
mReservedOp = reservedOp lexer

exprTable = [ [Prefix (mReservedOp "SET" >> return (Bin SET (Register "A"))) ] ]
