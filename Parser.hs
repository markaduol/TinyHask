module Parser where

import Lexer
import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity

---------------------------TOP LEVEL PARSERS--------------------------------

program_P :: Parser CoreProgram
program_P = semiSep1 scDefn_P

scDefn_P :: Parser CoreScDefn
scDefn_P = do
  (var:vars) <- many1 identifier
  reserved "="
  ex <- expr_P
  return (var, vars, ex)

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  res <- p
  eof
  return res

parseTopLevelProgram_P :: String -> Either ParseError CoreProgram
parseTopLevelProgram_P s = parse (contents program_P) "<stdin>" s
----------------------------------------------------------------------------

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
infixOp s f assoc = Ex.Infix (reservedOp s >> return f) assoc

-- Unary (prefix, postfix) and binary (infix) operators.
-- The list is ordered in descending order of precedence: that is,
-- all the operators in an inDivOpidual list have the same
-- precedence, but may different associativity.
opTable :: Ex.OperatorTable String () Identity CoreExpr
opTable = [
    [ infixOp "*" (EBinApp MulOp) Ex.AssocLeft
    , infixOp "/" (EBinApp DivOp) Ex.AssocLeft
    ],
    [ infixOp "+" (EBinApp AddOp) Ex.AssocLeft
    , infixOp "-" (EBinApp SubOp) Ex.AssocLeft
    ],
    [ infixOp ">"  (EBinApp GT_) Ex.AssocNone
    , infixOp ">=" (EBinApp GTE_) Ex.AssocNone
    , infixOp "<"  (EBinApp LT_) Ex.AssocNone
    , infixOp "<=" (EBinApp LTE_) Ex.AssocNone
    ],
    [ infixOp "==" (EBinApp EQ_) Ex.AssocLeft
    , infixOp "~=" (EBinApp NOT_) Ex.AssocLeft
    ],
    [ infixOp "&" (EBinApp AND_) Ex.AssocLeft],
    [ infixOp "|" (EBinApp OR_) Ex.AssocLeft],
    [ infixOp "" (EAp) Ex.AssocLeft]
  ]

-------------------------------------------------------------------------
----------------------------EXPRESSION PARSERS---------------------------

ident_P :: Parser CoreExpr
ident_P = do
  s <- identifier
  return (EVar s)

intLiteral_P :: Parser CoreExpr
intLiteral_P = do
  ds <- natural
  return (ENum (fromInteger ds))

dataConstr_P :: Parser CoreExpr
dataConstr_P = do
  reserved "Pack"
  reserved "{"
  tag <- lexeme natural
  reserved ","
  arity <- lexeme natural
  reserved "}"
  return (EConstr (fromInteger tag) (fromInteger arity))

letExpr_P :: Parser CoreExpr
letExpr_P = do
  isRec <- checkRec
  defns <- semiSep1 localDefn_P
  reserved "in"
  ex <- lexeme expr_P
  return (ELet isRec defns ex)
  where
    checkRec
      = (try (reserved "letrec") >> return True)
      <|> (reserved "let" >> return False)

caseExpr_P :: Parser CoreExpr
caseExpr_P = do
  reserved "case"
  ex <- lexeme expr_P
  reserved "of"
  alts <- semiSep1 alt_P
  return (ECase ex alts)

lambdaExpr_P :: Parser CoreExpr
lambdaExpr_P = do
  char '\\'
  vars <- many1 identifier
  reserved "."
  ex <- lexeme expr_P
  return (ELam vars ex)

localDefn_P :: Parser CoreDefn
localDefn_P = do
  var <- identifier
  reserved "="
  ex <- lexeme expr_P
  return (var, ex)

alt_P :: Parser CoreAlter
alt_P = do
  n <- angles natural
  vars <- many identifier
  reserved "->"
  ex <- lexeme expr_P
  return ((fromInteger n), vars, ex)

exprApp_Op :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
exprApp_Op = return EAp

plusOp :: Parser (CoreExpr -> CoreExpr -> CoreExpr)
plusOp = reserved "+" >> return (EBinApp AddOp)

expr_P :: Parser CoreExpr
expr_P = Ex.buildExpressionParser opTable expr_factor_P

expr_factor_P :: Parser CoreExpr
expr_factor_P
  =   parens expr_P
  <|> intLiteral_P
  <|> dataConstr_P
  <|> letExpr_P
  <|> caseExpr_P
  <|> lambdaExpr_P
  <|> ident_P
