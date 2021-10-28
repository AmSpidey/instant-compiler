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
instance Show Value where
    show (Reg reg) = reg
    show (Lit lit) = show lit
instance Show Expr where
    show (ValExpr valexpr) = show valexpr
    show (OpExpr Add val1 val2) = "add i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr Sub val1 val2) = "sub i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr Mul val1 val2) = "mul i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr Div val1 val2) = "sdiv i32 " ++ show val1 ++ ", " ++ show val2
instance Show LLVMStmt where
    show (Print val) = "call void @printInt(i32 " ++ show val ++ ")"
    show (Ass var val) = "store i32 " ++ show val ++ ", i32* " ++ varPtr var
    show (AssReg reg (ValExpr val)) = reg ++ " = load i32, i32* " ++ show val
    show (AssReg reg expr) = reg ++ " = " ++ show expr
instance Show LLVMRepr where
    show (Repr (x:xs)) = show x ++ "\n" ++ show (Repr xs)
    show (Repr []) = ""    
type LLVMM a = State Integer a

data LightExp = LExp Operand LightExp LightExp | LExpLit Integer | LExpVar Ident

prolog :: String
prolog = "declare i32 @printf(i8*, ...) \n\
\@dnl = internal constant [4 x i8] c\"%d\\0A\\00\" \n\
\define void @printInt(i32 %x) { \n\
\       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 \n\
\       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n\
\       ret void \n\
\}\n\
\define i32 @main() {\n"

epilog :: String
epilog = "ret i32 0\n}"

simplifyExp :: Exp -> LightExp
simplifyExp (ExpAdd exp1 exp2) = LExp Add (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpSub exp1 exp2) = LExp Sub (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpMul exp1 exp2) = LExp Mul (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpDiv exp1 exp2) = LExp Div (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpLit int) = LExpLit int
simplifyExp (ExpVar ident) = LExpVar ident

varPtr :: String -> String
varPtr var = "%ptr_" ++ var

seqReg :: Integer-> String 
seqReg n = "%reg_" ++ show n

compileLLVM :: Program -> String
compileLLVM p = prolog ++ declareVariables p ++ compileInternal p ++ epilog

declareVariables :: Program -> String
declareVariables (Prog stmts) =
    let vars = HS.toList $ collectVariables stmts
    in concatMap declare vars where
        declare :: String -> String
        declare var = varPtr var ++ " = alloca i32\n"

collectVariables :: [Stmt] -> HS.HashSet String
collectVariables stmts = HS.fromList $ concatMap collectFromStmt stmts where
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
    repr = (flip evalState 0 . internalRepr) stmts
    in show repr

internalRepr :: [Stmt] -> LLVMM LLVMRepr
internalRepr stmts = do
    lines <- mapM compileLine stmts
    return $ Repr $ concat lines

concatLines :: [LLVMM [LLVMStmt]] -> LLVMM [LLVMStmt]
concatLines (x:xs) = do
    y <- x
    return []
concatLines _ = undefined

compileLine :: Stmt -> LLVMM [LLVMStmt]
compileLine (SAss (Ident ident) exp) = do
    (prev, val) <- compileExp $ simplifyExp exp
    return $ prev ++ [Ass ident val]
compileLine (SExp exp) = do
    (prev, val) <- compileExp $ simplifyExp exp
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
    return ([AssReg (seqReg next_reg) $ ValExpr var_val], Reg (seqReg next_reg))