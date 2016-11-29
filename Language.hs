module Language where

import Syntax

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr e = case e of
  (EVar v) -> True
  (ENum n) -> True
  (EConstr n1 n2) -> True
  otherwise -> False

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], (EVar "x"))
  , ("K", ["x", "y"], (EVar "x"))
  , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
  ]

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []
