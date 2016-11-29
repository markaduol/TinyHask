module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = rNames
  , Tok.reservedOpNames = rOps
  , Tok.caseSensitive   = True
  }
  where
    -- List of reserved operators
    rOps =
      ["-", "*", "/", "+", ">", ">=", ">="
      , "<", "<=", "==", "~=", "&", "|"]
    -- List of reserved identifiers
    rNames =
      ["let", "in", "letrec", "case", "of"]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

natural :: Parser Integer
natural = Tok.natural lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

-- In Parser.hs, this function should only be called once explicitly, at the start of the main parsing function, in order to skip any leading whitespace
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
