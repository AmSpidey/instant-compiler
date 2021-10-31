module LLVMCompiler where
import AbsInstant
import Control.Monad.State
import qualified Data.HashSet as HS hiding (map)
import Common

type Var = String
type Register = String
data Value = Lit Integer | Reg Register
data Expr = OpExpr Operand Value Value | ValExpr Value
data LLVMStmt = Print Value | Ass Var Value | AssReg Register Expr
newtype LLVMRepr = Repr [LLVMStmt]
instance Show Value where
    show (Reg reg) = reg
    show (Lit lit) = show lit
instance Show Expr where
    show (ValExpr valexpr) = show valexpr
    show (OpExpr (COp Add) val1 val2) = "add i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr (NCop Sub) val1 val2) = "sub i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr (COp Mul) val1 val2) = "mul i32 " ++ show val1 ++ ", " ++ show val2
    show (OpExpr (NCop Div) val1 val2) = "sdiv i32 " ++ show val1 ++ ", " ++ show val2
instance Show LLVMStmt where
    show (Print val) = "call void @printInt(i32 " ++ show val ++ ")"
    show (Ass var val) = "store i32 " ++ show val ++ ", i32* " ++ varPtr var
    show (AssReg reg (ValExpr val)) = reg ++ " = load i32, i32* " ++ show val
    show (AssReg reg expr) = reg ++ " = " ++ show expr
instance Show LLVMRepr where
    show (Repr (x:xs)) = show x ++ "\n" ++ show (Repr xs)
    show (Repr []) = ""    
type LLVMM a = State Integer a

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

compileInternal :: Program -> String
compileInternal (Prog stmts) = let
    repr = (flip evalState 0 . internalRepr) stmts
    in show repr

internalRepr :: [Stmt] -> LLVMM LLVMRepr
internalRepr stmts = do
    lines <- mapM compileLine stmts
    return $ Repr $ concat lines

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