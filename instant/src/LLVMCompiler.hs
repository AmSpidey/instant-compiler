module LLVMCompiler where
import AbsInstant
import Control.Monad.State
import qualified Data.HashSet as HS hiding (map)

data Var = String
data Val = Lit Int | Var Var
data Expr = Add Val Val | Sub Val Val | Mul Val Val | Div Val Val | ExprVar Var
data Reg = String
data LLVMStmt = Print Reg | Ass Var Reg
newtype LLVMRepr = Repr [LLVMStmt]

varPtr :: String -> String
varPtr var = "%ptr_" ++ var

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
internalRepr stmts = Repr (concatMap compileLine stmts)

compileLine :: Stmt -> State Int [LLVMStmt]
compileLine (SAss ident exp) = do
    compileExp exp
    (last_reg, _) <- get
    Ass ident 
compileLine = undefined

compileExp :: Exp -> State Int [LLVMStmt]
compileExp = undefined 



