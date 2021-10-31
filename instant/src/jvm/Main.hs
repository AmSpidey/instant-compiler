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
import JVMCompiler ( compileJVM )

import ErrM

type ParseFun = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int


jasminPath :: String
jasminPath = "extras/jasmin.jar"

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
           Ok  tree ->    let basename = dropExtension f
                              compiled = compileJVM tree (takeFileName basename)
                          in
                          do let basename = dropExtension f
                                 j = addExtension basename ".j"
                                 dir = takeDirectory f
                             writeFile j compiled
                             cmd "java" ["-jar", jasminPath, "-d", dir, j]
                             cmd "mkdir" ["-p", dir </> "lib"]
                             cmd "cp" [jasminPath, dir </> "lib"]
                             exitSuccess

main :: IO ()
main = do
  fs <- getArgs
  mapM_ (runFile 2 pProgram) fs



