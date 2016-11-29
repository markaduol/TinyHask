module PrettyPrinter where

import Syntax
import Text.PrettyPrint

pprExpr :: CoreExpr -> Doc
pprExpr (ENum n) = int n
pprExpr (EVar v) = text v
pprExpr (EBinApp binOp e1 e2)
  = hcat
  [ pprExpr e1 <+> text (show binOp) <+> pprExpr e2]
pprExpr (ECase e alts) = empty --TODO
pprExpr (EConstr n1 n2) = empty --TODO
pprExpr (ELam vs e)
  = hcat
  [ text "\\"
  , foldl (\acc v -> acc <+> (text v)) empty vs
  , text " . "
  , pprExpr e
  ]
pprExpr (EAp e1 e2)
  = hsep [pprExpr e1, pprExpr e2]
pprExpr (ELet is_rec defns expr)
  = vcat
  [ text keyword
  , vcat (map pprDefn defns)
  , text "in"
  , pprExpr expr
  ]
  where
    keyword | is_rec     = "letrec"
            | not is_rec = "let"

pprDefn :: (Name, CoreExpr) -> Doc
pprDefn (name, expr) = hsep [text name, text "=", pprExpr expr]

pprScDefn :: CoreScDefn -> Doc
pprScDefn (name, vars, expr)
  = hsep
  [ text name
  , foldl (\acc v -> acc <+> (text v)) empty vars
  , text "="
  , pprExpr expr
  ]

pprProg :: CoreProgram -> Doc
pprProg scDefns = vcat (map pprScDefn scDefns)

pprint :: CoreProgram -> IO ()
pprint = print . pprProg
