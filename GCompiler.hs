module GCompiler where

import Utils

------ STATE ------

data GMState
  = GMState
  { code    :: GMCode
  , stack   :: GMStack
  , heap    :: GMHeap
  , globals :: GMGlobals
  , stats   :: GMStats
  } deriving (Show)

type GMCode    = [Instruction]
type GMStack   = [Addr]
type GMHeap    = Heap Node
type GMGlobals = Map.Map Name Addr
type GMStats   = Int

-- Returns address at specified index on the stack.
stackAt :: Int -> GMStack -> Int
stackAt n stack
  | length stack < n+1 = error $ "Index out of bounds: " ++ (show n)
  | otherwise          = (head . drop n) stack

initialStats :: GMStats
initialStats = 0

incStepStats :: GMStats -> GMStats
incStepStats s = s + 1

getStats :: GMStats -> Int
getStats s = s

------ INSTRUCTIONS ------

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int
  deriving (Show)

instance Eq Instruction where
  Unwind       == Unwind       = True
  Pushglobal a == Pushglobal b = a == b
  Pushint a    == Pushint b    = a == b
  Push a       == Push b       = a == b
  Mkap         == Mkap         = True
  Slide a      == Slide b      = a == b
  _            == _            = False

------ NODES ------

-- Nodes will be stored in the heap; the stack will contain addresses to various nodes.
data Node
  = NNum Int           -- Number
  | NAp Addr Addr      -- Application
  | NGlobal Int GmCode -- Global
  deriving (Show)

getArgAddr :: Node -> Addr
getArgAddr (NAp a b) = b
getArgAddr node      = error $ "In function: 'getArgAddr':\nNode " ++ (show node) ++ " is not an application node."

------ ARITHMETIC ------

data AExpr
  = Num Int
  | Plus AExpr AExpr
  | Mult AExpr AExpr
  deriving (Show)

data AInstruction
  = INum Int
  | IPlus
  | IMult
  deriving (Show)

------ EVALUATION ------

gmEval :: GMState -> [GMState]
gmEval st = st : sts
  where
    sts
      | gmFinal st = []
      | otherwise  = eval ((doAdmin . gmStep) st)
      where
        gmFinal :: GMState -> Bool
        doAdmin :: GMState -> GMState
        gmFinal st = (code st) == []
        doAdmin st = st {stats = incStepStats (stats st)}

gmStep :: GMState -> GMState
gmStep st = gmDispatch i (st {code = is})
  where
    (i:is) = code st

gmDispatch :: Instruction -> GMState -> GMState
gmDispatch (Pushglobal f) = gmPushGlobal f
gmDispatch (Pushint n)    = gmPushInt n
gmDispatch (Push n)       = gmPush n
gmDispatch Mkap           = gmMkap
gmDispatch (Slide n)      = gmSlide n
gmDispatch Unwind         = gmUnwind

gmPushGlobal :: Name -> GMState -> GMState
gmPushGlobal f st
  = st {stack = a : (stack st)}
  where
    a = case Map.lookup f (globals st) of
      Nothing   -> error $ "No global mapping for supercombinator: " ++ (show f)
      Just addr -> addr

gmPushInt :: Int -> GMState -> GMState
gmPushInt n st
  = st {stack = a : (stack st), heap = heap'}
  where
    (heap', a) = hAlloc (heap st) (NNum n)

gmPush :: Int -> GMState -> GMState
gmPush n st
  = st {stack = b : (stack st)}
  where
    b = getArgAddr (stackAt (n+1) (stack st))

gmMkap :: GMState -> GMState
gmMkap st
  = st {stack = a:as, heap = heap'}
  where
    (a1:a2:as) = stack st
    (heap', a) = hAlloc (heap st) (NAp a1 a2)

gmSlide :: Int -> GMState -> GMState
gmSlide n st
  = st {stack = a : (drop n as)}
  where
    (a:as) = stack st

gmUnwind :: GMState -> GMState
gmUnwind st
  = gmUnwind' (hLookup (heap st) a)
  where
    (a:as) = stack st
    gmUnwind' (NNum n)    = st
    gmUnwind' (NAp a1 a2) = st {code = [Unwind], stack = (a1 : a : as)}
    gmUnwind' (NGlobal n c)
      | length as < n = error "Too few arguments."
      | otherwise     = st {code = c}

------ ARITHMETIC ------

aInterpret :: AExpr -> Int
aInterpret (Num n)      = n
aInterpret (Plus a1 a2) = aInterpret a1 + aInterpret a2
aInterpret (Mult a1 a2) = aInterpret a1 * aInterpret a2

aCompile :: AExpr -> [AInstruction]
aCompile (Num n)      = [INum n]
aCompile (Plus a1 a2) = (aCompile a1) ++ (aCompile a2) ++ [IPlus]
aCompile (Mult a1 a2) = (aCompile a1) ++ (aCompile a2) ++ [IMult]

aEval :: ([AInstruction], [Int]) -> Int
aEval ([]         , [n])    = n
aEval (INum n : is, ns)     = aEval (i, n:ns)
aEval (IPlus :  is, m:n:ns) = aEval (i, (m + n) : ns)
aEval (IMult :  is, m:n:ns) = aEval (i, (m * n) : ns)
