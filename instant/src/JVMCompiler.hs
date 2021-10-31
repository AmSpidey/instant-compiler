module JVMCompiler where
import AbsInstant
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashSet as HS hiding (map)
import qualified Data.HashMap as HM
import Common
import Data.HashMap (HashMap)
import Data.Ix(inRange)

newtype FlexInt = FInt Integer
data JVMStmt = PrePrint | Print | Op Operand | Load FlexInt | Store FlexInt | Push Integer
newtype JVMRepr = Repr [JVMStmt]
instance Show FlexInt where
    show (FInt int)
        | inRange (0, 3) int = "_" ++ show int
        | otherwise = " " ++ show int
instance Show JVMStmt where
    show PrePrint = "getstatic java/lang/System/out Ljava/io/PrintStream;"
    show Print = "invokevirtual java/io/PrintStream/println(I)V"
    show (Op Add) = "iadd"
    show (Op Mul) = "imul"
    show (Op Div) = "idiv"
    show (Op Sub) = "isub"
    show (Load int) = "iload" ++ show int
    show (Store int) = "istore" ++ show int
    show (Push int)
        | inRange (0, 5) int = "iconst_" ++ show int
        | inRange (0, 2^7 - 1) int = "bipush " ++ show int
        | inRange (2^9, 2^15 - 1) int = "sipush " ++ show int
        | otherwise = "ldc " ++ show int
instance Show JVMRepr where
    show (Repr (x:xs)) = show x ++ "\n" ++ show (Repr xs)
    show (Repr []) = ""    
type Env = HM.Map String Integer
type JVMM a = Reader Env a

prolog :: String -> String
prolog classname = ".class public " ++ classname ++ "\n.super java/lang/Object\n\n\
\.method public static main([Ljava/lang/String;)V\n"

epilog :: String
epilog = "return\n.end method"

compileJVM :: Program -> String -> String
compileJVM p classname = let
    env = createEnvironment p
    (stmts, stack_size) = optimizeStack $ simplifyProg p
    in prolog classname ++ declareStackSize stack_size ++ declareVariables (HM.size env) ++ show (runReader (compileInternal stmts) env) ++ epilog

declareStackSize :: Integer -> String
declareStackSize k = ".limit stack " ++ show k ++ "\n"

declareVariables :: Int -> String
declareVariables n = ".limit locals " ++ show (n + 1) ++ "\n" ++ concatMap declare [1..n] where
        declare :: Int -> String
        declare k = "iconst_0\nistore"  ++ show (FInt $ fromIntegral k) ++ "\n"

optimizeStack :: [LightStmt] -> ([LightStmt], Integer)
optimizeStack stmts = let
    (oe, s) = unzip $ map optimizeStackStmt stmts
    in (oe, maximum s)

optimizeStackStmt :: LightStmt -> (LightStmt, Integer)
optimizeStackStmt (LSAss ident exp) = let
    (oe, s) = optimizeExp exp
    in (LSAss ident oe, s)
optimizeStackStmt (LSExp exp) = let
    (oe, s) = optimizeExp exp
    in (LSExp exp, s + 1) -- +1 for the PrintStream

optimizeExp :: LightExp -> (LightExp, Integer)
optimizeExp (LExpLit int) = (LExpLit int, 1)
optimizeExp (LExpVar indent) = (LExpVar indent, 1)
optimizeExp (LExp op exp1 exp2) = let
    (oe1, s1) = optimizeExp exp1
    (oe2, s2) = optimizeExp exp2
    in if s1 > s2 then (LExp op oe1 oe2, s1)
        else (LExp op oe2 oe1, s2 + 1)

createEnvironment :: Program -> Env
createEnvironment (Prog stmts) =
    let vars = HS.toList $ collectVariables stmts
        env = HM.fromList $ zip vars [1..]
    in env

compileInternal :: [LightStmt] -> JVMM JVMRepr
compileInternal stmts = do
    (lines, stack_sizes) <- unzip <$> mapM compileLine stmts
    return $ Repr $ concat lines

-- Result: (statements, stack size)
compileLine :: LightStmt -> JVMM ([JVMStmt], Integer)
compileLine (LSAss (Ident ident) exp) = do
    (exp, s) <- compileExp exp
    env <- ask
    return (exp ++ [Store $ FInt $ HM.findWithDefault (-1) ident env], s)
compileLine (LSExp exp) = do
    (exp, s) <- compileExp exp
    return ([PrePrint] ++ exp ++ [Print], s)

compileExp :: LightExp -> JVMM ([JVMStmt], Integer)
compileExp (LExp op exp1 exp2) = do
    (exp1m, s1) <- compileExp exp1
    (exp2m, s2) <- compileExp exp2
    return $ if s1 > s2 then (exp1m ++ exp2m ++ [Op op], s1) 
    else (exp2m ++ exp1m ++ [Op op], s2)
compileExp (LExpLit int) = return ([Push int], 1)
compileExp (LExpVar (Ident ident)) = do
    env <- ask 
    return ([Load $ FInt$ HM.findWithDefault (-1) ident env], 1)