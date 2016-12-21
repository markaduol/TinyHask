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
  = NAp Addr Addr
  | NSupercomb Name [a] (Expr a)
  | NNum Int
  deriving (Show)

-- A stack is represented as a list of addresses.
type TiStack = [Addr]

data TiDump = DummyTiDump deriving (Show)

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

initial_tidump :: TiDump
initial_tidump = DummyTiDump

initial_stats  :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

initial_stats    = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

---------------------------- UTILITY FUNCTIONS ---------------------------------

-- Compiler takes a program, and from it, creates the initial state of the machine.
compile :: CoreProgram -> TiState
compile program
  = TiState initial_stack initial_tidump initial_heap globals initial_stats
  where
    sc_defs                 = program ++ preludeDefs ++ extraPreludeDefs
    (initial_heap, globals) = buildInitialHeap sc_defs
    initial_stack           = [address_of_main]
    address_of_main         = case Map.lookup "main" globals of
      Just addr -> addr
      Nothing   -> error "main is not defined."

-- TODO: Parallelise
buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs
  = transToMapGlobals (List.mapAccumL allocate_sc hInitial sc_defs)
  where
    transToMapGlobals :: (TiHeap, [(Name, Addr)]) -> (TiHeap, Map.Map Name Addr)
    transToMapGlobals (tiHeap, globals)
      = (tiHeap, Map.fromList globals)
    allocate_sc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
    allocate_sc heap (sc_name, sc_args, sc_body)
      = (heap', (sc_name, allocatedAddr))
      where
        (heap', allocatedAddr) = hAlloc heap (NSupercomb sc_name sc_args sc_body)
