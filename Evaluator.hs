module Evaluator where

import Syntax
import Language
import PrettyPrinter
import Utils
import Compiler

import Data.Map as Map
import Data.List as List
import Control.Monad.State
import Text.PrettyPrint as PP

----------------------------------- USAGE ------------------------------------
-- Basically, the 'TiMachine' abstract data type acts as a wrapper for the
-- state transition system.

-- 'State s a' is actually a synonym for the monad state transformer with the
-- identity monad: 'StateT s Identity a'.
-- It therefore simply acts as an extra wrapper for the actual 'state'
-- and 'result' abstract data types, represented by 's' and 'a' respectively.

-- We can run 'State s a' with the function 'runState :: State s a -> s -> (a, s)',
-- given some initial state 's'.

-- 'core_prog' has type 'CoreProgram'

-- (showResults . fst . eval . compile) core_prog
-------------------------------------------------------------------------------

-- Evaluate compiled program from initial state and return all states from
-- initial state to final state.
eval :: TiState -> ([TiState], TiState)
eval st = (st : sts, final_st)
  where
    (sts, final_st)
      | isFinalSt st = ([], st)
      | otherwise    = eval (step st)

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

showResults :: [TiState] -> IO ()
showResults = print . PP.vcat . List.map showState

-- We only show the stack.
showState :: TiState -> Doc
showState (TiState stack dump heap globals stats)
  = showStack heap stack <> (PP.text "\n")

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack
  = hcat
  [ PP.text "Stack:\n"
  , (PP.brackets . PP.nest 4 . PP.hcat) (PP.punctuate (PP.text "\n") (List.map showStackItem stack))
  ]
  where
    showStackItem addr
      = PP.hcat [showAddr addr , PP.text ": ", showStackNode heap (hLookup heap addr)]

showAddr :: Addr -> Doc
showAddr addr = PP.text ("#" ++ (show addr))

showStackNode :: Heap (Node a) -> Node a -> Doc
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
