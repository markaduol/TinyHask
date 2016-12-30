{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler where

import Parser
import Syntax
import Language
import PrettyPrinter
import Utils
import Data.Map as Map
import Data.List as List
import Control.Monad.State
import Data.Functor.Identity

----------------------------- BASIC DATA TYPES ---------------------------------

-- An application node is represented as two addresses
-- A supercombinator node holds its name, arguments and an expression body
-- A number node is simply represented as an integer.
data Node a
  = NAp Addr Addr                -- Application
  | NSupercomb Name [a] (Expr a) -- Supercombinator
  | NNum Int                     -- Number
  | NInd Addr                    -- Indirection
  | NPrim Name Primitive         -- Primitive
  | NData Int [Addr]             -- Constructor ID (tag) and list of addresses to nodes holding data
  deriving (Show)

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | Cond -- Needed for "if-else" statements.
  | LT_Prim
  | LEQ_Prim
  | EQ_Prim
  | GEQ_Prim
  | GT_Prim
  | PrimConstr Int Int deriving (Show)

-- A stack is represented as a list of addresses.
type TiStack = [Addr]

type TiDump = [TiStack]

-- TiHeap is represented as a heap that contains objects of type Node
type TiHeap = Heap (Node Name)

-- Associates each supercombinator name with a heap address containing it's definition.
type TiGlobals = Map.Map Name Addr

type TiStats = Int

----------------------------- TI-MACHINE STATE ---------------------------------

data TiState
  = TiState
  { stack   :: TiStack   -- Stack of addresses, each of which identifies a node in the heap.
  , dump    :: TiDump    -- Records state of stack, prior to evaluation of argument.
  , heap    :: TiHeap    -- Mapping of addresses to nodes.
  , globals :: TiGlobals -- Gives the address of each heap node representing each supercombinator/prmitive.
  , stats   :: TiStats   -- Statistics on what the machine does.
  } deriving (Show)

-------------------------------- DEPRECATED ----------------------------------------
newtype TiMachine a = TiMachine {runTiMachine :: State TiState a}
  deriving (MonadState TiState)

instance Functor TiMachine where
  fmap f k = k >>= (pure . f)

instance Applicative TiMachine where
  pure = TiMachine . pure
  j <*> k = j >>= \f ->
            k >>= (pure . f)

instance Monad TiMachine where
  k >>= f = state $ \s ->
    let (a, s') = runState (runTiMachine k) s
    in runState (runTiMachine (f a)) s'
-----------------------------------------------------------------------------------

--------------------------- TI-MACHINE INITIIALISATION -------------------------

initialDump :: TiDump
initialDump = []

initialStats  :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

initialStats     = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

---------------------------- UTILITY FUNCTIONS ---------------------------------

-- If we have multiple definitions of a supercombinator, the definition furthest
-- to the RHS of 'addressOfMain' takes precedence.

-- Compiler takes a program, and from it, creates the initial state of the machine.
compile :: CoreProgram -> TiState
compile program
  = TiState initialStack initialDump initialHeap globals initialStats
  where
    scDefs                 = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack           = [addressOfMain]
    addressOfMain          = case Map.lookup "main" globals of
      Just addr -> addr
      Nothing   -> error "main is not defined."

-- TODO: Parallelise
buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs
  = (heap', Map.fromList $ scAddrs ++ primAddrs)
  where
    (heap, scAddrs)    = (List.mapAccumL allocateSC hInitial scDefs)
    (heap', primAddrs) = (List.mapAccumL allocatePrim heap primDefs)

allocateSC :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSC heap (scName, scArgs, scBody)
  = (\(h, addr) -> (h, (scName, addr))) $ hAlloc heap (NSupercomb scName scArgs scBody)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (primName, prim)
  = (\(h, addr) -> (h, (primName, addr))) $ hAlloc heap (NPrim primName prim)

primDefs :: [(Name, Primitive)]
primDefs
  =
  [ ("negate", Neg)
  , ("+", Add)
  , ("-", Sub)
  , ("/", Div)
  , ("*", Mul)
  , ("if", Cond)
  , ("False", PrimConstr 1 0)
  , ("True", PrimConstr 2 0)
  , ("<", LT_Prim)
  , ("<=", LEQ_Prim)
  , ("==", EQ_Prim)
  , (">=", GEQ_Prim)
  , (">", GT_Prim)
  ]
