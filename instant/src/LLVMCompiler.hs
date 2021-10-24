module LLVMCompiler where
import AbsInstant
import Control.Monad.State
import qualified Data.HashSet as HS hiding (map)
import Numeric (Floating(expm1))

type Var = String
type Register = String
data Value = Lit Integer | Reg Register
data Operand = Add | Sub | Mul | Div
data Expr = OpExpr Operand Value Value | ValExpr Value
data LLVMStmt = Print Value | Ass Var Value | AssReg Register Expr
newtype LLVMRepr = Repr [LLVMStmt]
instance Show LLVMRepr where
    show = undefined
type LLVMM a = State Integer a

data LightExp = LExp Operand LightExp LightExp | LExpLit Integer | LExpVar Ident

simplifyExp :: Exp -> LightExp
simplifyExp (ExpAdd exp1 exp2) = LExp Add (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpSub exp1 exp2) = LExp Add (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpMul exp1 exp2) = LExp Add (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpDiv exp1 exp2) = LExp Add (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpLit int) = LExpLit int
simplifyExp (ExpVar ident) = LExpVar ident

varPtr :: String -> String
varPtr var = "%ptr_" ++ var

seqReg :: Integer-> String 
seqReg n = "%reg_" ++ show n

compileLLVM :: Program -> String
compileLLVM p = declareVariables p ++ compileInternal p

declareVariables :: Program -> String
declareVariables (Prog stmts) =
    let vars = HS.toList $ collectVariables stmts
    in concatMap declare vars where
        declare :: String -> String
        declare var = varPtr var ++ " = alloca i32"

collectVariables :: [Stmt] -> HS.HashSet String
collectVariables stmts = HS.fromList $ map (concat . collectFromStmt) stmts where
    collectFromStmt :: Stmt -> [String]
    collectFromStmt (SAss (Ident ident) exp) = ident : collectFromStmt (SExp exp)
    collectFromStmt (SExp exp) = collectFromExpr exp where
        collectFromExpr (ExpAdd exp1 exp2) = collectFromExpr exp1 ++ collectFromExpr exp2
        collectFromExpr (ExpSub exp1 exp2) = collectFromExpr exp1 ++ collectFromExpr exp2
        collectFromExpr (ExpMul exp1 exp2) = collectFromExpr exp1 ++ collectFromExpr exp2
        collectFromExpr (ExpDiv exp1 exp2) = collectFromExpr exp1 ++ collectFromExpr exp2
        collectFromExpr (ExpVar (Ident ident)) = [ident]
        collectFromExpr (ExpLit _) = []

compileInternal :: Program -> String
compileInternal (Prog stmts) = let
    repr = internalRepr stmts
    in show repr

internalRepr :: [Stmt] -> LLVMRepr
internalRepr stmts = Repr (concatMap (flip evalState 0 . compileLine) stmts)

compileLine :: Stmt -> LLVMM [LLVMStmt]
compileLine (SAss (Ident ident) exp) = do
    (prev, val) <- compileExp $ simplifyExp exp
    next_reg <- get
    return $ prev ++ [Ass ident val]
compileLine (SExp exp) = do
    (prev, val) <- compileExp $ simplifyExp exp
    next_reg <- get
    return $ prev ++ [Print val]

compileExp :: LightExp -> LLVMM ([LLVMStmt], Value)
compileExp (LExp op exp1 exp2) = do
    (exp1m, exp1val) <- compileExp exp1
    (exp2m, exp2val) <- compileExp exp2
    next_reg <- get
    put $ next_reg + 1
    return (exp1m ++ exp2m ++ [AssReg (seqReg next_reg) $ OpExpr op exp1val exp2val], Reg (seqReg next_reg))
compileExp (LExpLit int) = return ([], Lit int)
compileExp (LExpVar (Ident ident)) = do
    next_reg <- get
    put $ next_reg + 1
    let var_val = Reg $ varPtr ident
    return ([AssReg (seqReg next_reg) $ ValExpr var_val], var_val)