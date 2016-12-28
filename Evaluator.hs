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

-- | Evaluates compiled program from initial state and returns all states.
eval :: TiState -> ([TiState], TiState)
eval st = (st : sts, finalSt)
  where
    (sts, finalSt)
      | isFinalSt st = ([], st)
      | otherwise    = eval (step st)

isFinalSt :: TiState -> Bool
isFinalSt st
  | dump st == []
    && (length (stack st) == 1) = isDataNode stackNode
  | stack st == []              = error "Empty Stack!"
  | otherwise                   = False
  where
    stackNode = hLookup (heap st) (head $ stack st)

isDataNode :: Node a -> Bool
isDataNode node = case node of
  NNum n    -> True
  otherwise -> False

-------- TRANSITIONS ------

-- | Performs state transition
step :: TiState -> TiState
step st = case hLookup (heap st) (head $ stack st) of
  NAp a1 a2               -> apStep st a1 a2
  NSupercomb sc args body -> scStep st sc args body
  NNum n                  -> numStep st n
  NInd a                  -> indStep st a
  NPrim name prim         -> primStep st prim

------- PRIMITIVE NODE TRANSITION ----------
primStep :: TiState -> Primitive -> TiState
primStep st prim = case prim of
  Add -> primArith st (+)
  Sub -> primArith st (-)
  Mul -> primArith st (*)
  Div -> primArith st (div)
  Neg -> primArith' st ((-) 0)

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith st f = st {stack = stack', dump = dump', heap = heap'}
  where
    redexRootAddr          = (stack st) !! 2
    argAddrs               = Prelude.map (getArgAddr (heap st)) (take 2 . drop 1 $ stack st)
    argNodes               = Prelude.map (hLookup (heap st)) argAddrs
    (stack', dump', heap') = reduceArgNodes argNodes
      where
        stack''           = drop (length argNodes) (stack st)
        res               = foldl1 f (List.map (\node -> let (NNum x) = node in x) argNodes)
        reduceArgNodes [] = (stack'', dump st, hUpdate (heap st) redexRootAddr (NNum res))
        reduceArgNodes (x:xs)
          | isDataNode x = reduceArgNodes xs
          | otherwise    = ([argAddr'], argAddrs' : stack'' : (dump st), heap st)
          where
            (argAddr':argAddrs') = drop (length argAddrs - length (x:xs)) argAddrs

primArith' :: TiState -> (Int -> Int) -> TiState
primArith' st f = st {stack = stack', dump = dump', heap = heap'}
  where
    redexRootAddr = (stack st) !! 1
    argAddr       = getArgAddr (heap st) redexRootAddr
    argNode       = hLookup (heap st) argAddr
    (stack', dump', heap')
      | isDataNode argNode = (stack'', dump st, hUpdate (heap st) redexRootAddr (NNum res))
      | otherwise          = ([argAddr], stack'' : (dump st), heap st)
      where
        stack'' = drop 1 (stack st)
        res     = let (NNum x) = argNode in f x

---- NUMBER NODE TRANSITION -----
numStep :: TiState -> Int -> TiState
numStep st n
  | length (stack st) == 1
    && (dump st /= []) = st {stack = head (dump st), dump = tail (dump st)}
  | otherwise          = error "A number has been applied as a function."

------- INDIRECTION NODE TRANSITION --------

indStep :: TiState -> Addr -> TiState
indStep st a = st {stack = a : drop 1 (stack st)}

------- APPLICATION NODE TRANSITION ---------

-- We reduce to the left since arguments of supercombinators
-- are always on the right-hand side of an application node.
apStep :: TiState -> Addr -> Addr -> TiState
apStep st a1 a2 = st {stack = a1 : (stack st)}

------- SUPERCOMBINATOR NODE TRANSITION ---------

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep st sc args body = st {stack = stack', heap = heap'}
  where
    redexRoot = (stack st) !! (length args)
    heap'     = instantiateAndUpdate body redexRoot (heap st) env
    stack'    = drop (length args) (stack st)
    env       = Map.union (Map.fromList argBindings) (globals st)
    -- Bind argument names to addresses obtained from the stack and heap.
    argBindings = zip args (getArgAddrs (heap st) (stack st))
      where
        getArgAddrs :: TiHeap -> TiStack -> [Addr]
        getArgAddrs heap (scNameAddr : stack) = Prelude.map (getArgAddr heap) stack

getArgAddr :: TiHeap -> Addr -> Addr
getArgAddr heap addr = let (NAp funcAddr argAddr) = hLookup heap addr in argAddr

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap

instantiateAndUpdate (EAp expr1 expr2) updAddr heap env
  = hUpdate heap2 updAddr (NAp a1 a2)
  where
    (heap1, a1) = instantiate expr1 heap env
    (heap2, a2) = instantiate expr2 heap1 env

instantiateAndUpdate (ENum n) updAddr heap env
  = hUpdate heap updAddr (NNum n)

instantiateAndUpdate (EVar v) updAddr heap env
  = case Map.lookup v env of
      Nothing      -> error $ "Undefined reference to variable: " ++ (show v)
      Just varAddr -> hUpdate heap updAddr (NInd varAddr)

instantiateAndUpdate (ELet is_rec defns expr) updAddr heap env
  = instantiateAndUpdate expr updAddr heap'' env''
  where
    (heap'', env'')
      = instantiateDefs heap env defns
    instantiateDefs heap env []
      = (heap, env)
    instantiateDefs heap env ((name, expr) : xs)
      = let
      { (heap', defnAddr) = instantiate expr heap env
      ; env'              = Map.insert name defnAddr env
      } in instantiateDefs heap' env' xs

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

instantiate (EAp expr1 expr2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate expr1 heap env
    (heap2, a2) = instantiate expr2 heap1 env

instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env

instantiate (ELet is_rec defns expr) heap env
  = instantiateLet defns expr heap env

instantiate e heap env
  = error "Can't instantiate binary expressions, case expressions or lambda expressions."

instantiateConstr :: Int -> Int -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateConstr tag arity heap env
  = error "Can't instantiate constructor expressions yet."

instantiateLet ::  [CoreDefn] -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet defns expr heap env
  = instantiate expr heap'' env''
  where
    (heap'', env'')                            = instantiateDefs heap env defns
    instantiateDefs heap env []                = (heap, env)
    instantiateDefs heap env ((name, expr):xs) = let {
      (heap', defnAddr) = instantiate expr heap env'; -- Mutually recursive, to deal with recursive let bindings.
      env'              = Map.insert name defnAddr env;
    } in instantiateDefs heap' env' xs

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

-- We show the value of the argument node
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
showNode (NInd a)
  = PP.hsep [PP.text "NInd", showAddr a]
