module Evaluator where

import Syntax
import Language
import PrettyPrinter
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
          | tiFinal st = ([], st)
          | otherwise  = eval' (step st)

tiFinal :: TiState -> Bool
tiFinal (TiState [addr] _ heap _ _) = isDataNode (hLookup heap addr)
tiFinal (TiState [] _ _ _ _)        = error "Empty Stack!"
tiFinal state                       = False

isDataNode :: Node a -> Bool
isDataNode (NNum n) = True
isDataNode node     = False

step :: TiState -> TiState
step st = case hLookup (heap st) (head $ stack st) of
  NAp a1 a2               -> apStep st a1 a2
  NSupercomb sc args body -> scStep st sc args body
  NNum n                  -> numStep st n

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
    arg_bindings
      = zip args (getArgAddrs (heap st) (stack st))

-- Gets address of each argument of a supercombinator
-- The address argument is found on the RHS of each application node
-- Pre: Stack contains sequential list of addresses of application nodes of
--      supercombinator (from deepest in tree to highest).
getArgAddrs :: TiHeap -> TiStack -> [Addr]
getArgAddrs heap (sc_name_addr : stack)
  = Prelude.map getArgAddr stack
  where
    getArgAddr addr = arg_addr
      where
        (NAp func arg_addr) = hLookup heap addr

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
    (heap2, a2) = instantiate e2 heap env
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
