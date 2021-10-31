module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath
import SimpleCmd ( cmd )

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import LLVMCompiler ( compileLLVM )

import ErrM

type ParseFun = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p f

run :: Verbosity -> ParseFun -> FilePath -> String -> IO ()
run v p f s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree ->    let compiled = compileLLVM tree
                          in
                          do let basename = dropExtension f
                                 ll = addExtension basename ".ll"
                                 bc = addExtension basename ".bc"
                             writeFile ll compiled
                             cmd "llvm-as" ["-o", bc, ll]
                             exitSuccess

main :: IO ()
main = do
  fs <- getArgs
  mapM_ (runFile 2 pProgram) fs



