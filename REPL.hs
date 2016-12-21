module REPL where

import Parser
import Syntax
import PrettyPrinter
import Compiler
import Evaluator

import System.Console.Haskeline
import Control.Monad.Trans
import System.IO

processProgram :: String -> IO ()
processProgram input = do
  case parseTopLevelProgram_P input of
    Left err      -> print err
    Right scDefns -> pprint scDefns

readInFile :: String -> IO ()
readInFile path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  processProgram contents
  putStrLn "\n"
  hClose handle

readInProgram :: String -> IO ()
readInProgram path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  case parseTopLevelProgram_P contents of
    Left err      -> print err
    Right sc_defs -> (showResults . fst . eval . compile) sc_defs
  putStrLn "\n"
  hClose handle

putAST :: String -> IO ()
putAST path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  case parseTopLevelProgram_P contents of
    Left err      -> print err
    Right scDefns -> mapM_ print scDefns
  hClose handle
