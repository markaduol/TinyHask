module Utils where

import Data.Map as Map

--------------------------- HEAP INTERFACE -----------------------------

-- A heap is a collection of unique objects of type 'a', each associated with a
-- unique address of type 'Addr'.
-- The following behaviours must be provided by the heap.

-- Initialises a heap.
hInitial :: Heap a
-- Allocates an object 'a' to the heap, returning the new heap and the address
-- to which the object is allocated.
hAlloc :: Heap a -> a -> (Heap a, Addr)
-- Updates the address of an object 'a' in the heap to the new given address.
-- Precondition: Address is mapped to an object in the heap
hUpdate :: Heap a -> Addr -> a -> Heap a
-- Frees the given address in the heap, removing any objects that may be
-- allocated at that particular address in the heap.
-- Precondition: Address is mapped to an object in the heap
hFree :: Heap a -> Addr -> Heap a
-- Retrieves the object allocated to a particular address in the heap.
-- If no objects are allocated to that particular address, we throw an error.
hLookup :: Heap a -> Addr -> a
-- Retrieves the addresses of all objects in the heap.
hAddresses :: Heap a -> [Addr]
-- Retrieves the number of objects in the heap.
hSize :: Heap a -> Int
-- An address guaranteed to differ from every address returned by 'hAlloc'.
hNull :: Addr
-- Check whether the given address is equal to 'hNull'.
hIsNull :: Addr -> Bool
-- Shows addresses as an array of characters. This is useful for showing
-- addresses in different formats.
showAddr :: Addr -> [Char]

------------------------------ IMPLEMENTATION ------------------------------
data Heap a
  = Heap
  { numObjects      :: Int            -- Number of objects in the heap.
  , unusedAddresses :: [Int]          -- List of unuses addresses.
  , mapping         :: Map.Map Addr a -- Mapping of addresses to objects.
  } deriving (Show)

type Addr = Int

hInitial
  = Heap 0 [1..addrLimit] Map.empty
  where
    addrLimit = 0xFF

hAlloc heap obj
  = let (addr:addrs) = (unusedAddresses heap)
        newMap =  Map.insert addr obj (mapping heap)
        n = (numObjects heap) + 1
    in (Heap n addrs newMap, addr)

hUpdate heap addr obj
  = let newMap = Map.insert addr obj (mapping heap)
    in (Heap (numObjects heap) (unusedAddresses heap) newMap)

hFree heap addr
  = let newMap = Map.delete addr (mapping heap)
    in Heap (numObjects heap) (addr : unusedAddresses heap) newMap

hLookup heap addr
  = case Map.lookup addr (mapping heap) of
    Nothing  -> error "Heap address lookup returned Nothing!"
    Just obj -> obj

hAddresses heap
  = Map.keys (mapping heap)

hSize heap
  = numObjects heap

hNull = 0

hIsNull addr = addr == hNull

showAddr addr = "#" ++ (show addr)
