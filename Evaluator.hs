module Evaluator where

import Syntax
import Language
import PrettyPrinter as PP
import Utils
import Compiler
import Data.Map as Map
import Data.List as List
import Control.Monad.State

-- Return all states from initial state to final state
eval :: TiMachine [TiState]
eval = state eval'
  where
    eval' :: TiState -> ([TiState], TiState)
    eval' st = (st : sts, final_st)
      where
        (sts, final_st)
          | isFinalSt st = ([], st)
          | otherwise  = eval' (step st)

isFinalSt :: TiState -> Bool
isFinalSt st = case st of
  TiState [addr] _ heap _ _ -> isDataNode (hLookup heap addr)
  TiState [] _ _ _ _        -> error "Empty Stack!"
  otherwise                 -> False

isDataNode :: Node a -> Bool
isDataNode node = case node of
  NNum n    -> True
  otherwise -> False

-- First, perform a lookup in the heap for the address at the head of the stack.
step :: TiState -> TiState
step st = case hLookup (heap st) (head $ stack st) of
  NAp a1 a2               -> apStep st a1 a2
  NSupercomb sc args body -> scStep st sc args body
  NNum n                  -> numStep st n

-- We reduce to the left since arguments of supercombinators
-- are always on the right-hand side of an application node.
apStep :: TiState -> Addr -> Addr -> TiState
apStep st a1 a2
  = st {stack = a1 : (stack st)}

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep st sc args body
  = st {stack = new_stack, heap = new_heap}
  where
    (new_heap, result_addr)
      = instantiate body (heap st) globals_env
    new_stack
      = result_addr : (drop (length args + 1) (stack st))
    globals_env
      = Map.union (Map.fromList arg_bindings) (globals st)
    arg_bindings -- Bind argument names to addresses obtained from the stack and heap.
      = zip args (getArgAddrs (heap st) (stack st))

-- Gets address of each argument of a supercombinator
-- The address argument is found on the right-hand side of each application node
-- Pre: Stack contains sequential list of addresses of application nodes of
--      supercombinator (from deepest in tree to highest).
getArgAddrs :: TiHeap -> TiStack -> [Addr]
getArgAddrs heap (sc_name_addr : stack)
  = Prelude.map getArgAddr stack
  where
    getArgAddr addr = let (NAp func_addr arg_addr) = hLookup heap addr in arg_addr

-- Takes an expression, heap and environment associating names to addresses and
-- creates an instance of the expression on the heap, returning the root of this
-- instance. This function performs the necessary expression reduction on the
-- graph.
instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EVar v) heap env
  = case Map.lookup v env of
    Nothing -> error $ "Undefined reference to variable: " ++ (show v)
    Just a  -> (heap, a)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env
instantiate (ELet is_rec defns e) heap env
  = instantiateLet is_rec defns e heap env
instantiate e heap env
  = error "Can't instantiate binary expressions, case expressions or lambda expressions."

instantiateConstr :: Int -> Int -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateConstr tag arity heap env
  = error "Can't instantiate constructor expressions yet."

instantiateLet :: IsRec -> [CoreDefn] -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet is_rec defns e heap env
  = error "Can't instantiate case expressions yet."

numStep :: TiState -> Int -> TiState
numStep st n = error "It appears that a number has been applied as a function."

------------------------------ FOR SHOWING RESULTS -----------------------------

showResults :: TiMachine [TiState] -> IO ()
showResults result_states = do
  sts <- result_states
  mapM_ (print . showState) sts

-- We only show the stack.
showState :: TiState -> Doc
showState (TiState stack dump heap globals stats)
  = showStack heap stack <> (PP.text "\n")

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack
  = hcat
  [ PP.text "Stack:\n"
  , (PP.brackets . PP.nest 4 . PP.hcat) (PP.punctuate (PP.text "\n") (map showStackItem stack))
  ]
  where
    showStackItem addr
      = PP.hcat [showAddr addr , PP.text ": ", showStackNode heap (hLookup heap addr)]

showAddr :: Addr -> Doc
showAddr = PP.text . show

showStackNode :: Heap a -> Node a -> Doc
showStackNode heap (NAp func_addr arg_addr)
  = PP.hsep
  [ PP.text "NAp"
  , showAddr func_addr
  , showAddr arg_addr
  , (PP.parens . showNode) (hLookup heap arg_addr)
  ]
showStackNode heap node = showNode node

-- Shows the value of a node
showNode :: Node a -> Doc
showNode (NAp a1 a2)
  = PP.hsep [PP.text "NAp", showAddr a1, showAddr a2]
showNode (NSupercomb name args body)
  = PP.hsep [PP.text "NSupercomb", PP.text name]
showNode (NNum n)
  = PP.hsep [PP.text "NNum", PP.int n]
