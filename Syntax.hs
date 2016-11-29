module Syntax where

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], (Expr a))
type CoreScDefn = ScDefn Name

type Name = String
type IsRec = Bool

-- TODO: Should we make application left-associative by using atomic expressions?
data Expr a
  = EAp (Expr a) (Expr a) -- Application
  | EBinApp BinOp (Expr a) (Expr a) -- Binary application
  | ELet IsRec [Defn a] (Expr a) -- Local definition
  | ECase (Expr a) [Alter a] -- Case expression
  | ELam [a] (Expr a) -- Lambda abstraction
  | EVar Name -- Variables
  | ENum Int  -- Numbers
  | EConstr Int Int -- Constructor with tag and arity
  deriving (Show)

type CoreExpr = Expr Name

type Defn a = (a, Expr a)
type CoreDefn = Defn Name

type Alter a = (Int, [a], (Expr a))
type CoreAlter = Alter Name

data BinOp
  = Mul | Div | Add | Sub | GT_ | GTE_ | LT_
  | LTE_ | EQ_ | NOT_ | AND_ | OR_

instance Show BinOp where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show GT_ = ">"
  show GTE_ = "<"
  show LT_ = "<"
  show LTE_ = "<="
  show EQ_ = "=="
  show NOT_ = "!"
  show AND_ = "&&"
  show OR_ = "||"
