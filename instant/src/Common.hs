module Common where
import AbsInstant
import qualified Data.HashSet as HS hiding (map)

data COp = Add | Mul
data NCop = Div | Sub
data Operand = COp COp| NCop NCop
data LightExp = LExp Operand LightExp LightExp | LExpLit Integer | LExpVar Ident
data LightStmt = LSAss Ident LightExp | LSExp LightExp

simplifyProg :: Program -> [LightStmt]
simplifyProg  (Prog stmts) = map simplifyStmt stmts

simplifyStmt :: Stmt -> LightStmt
simplifyStmt (SAss id exp) = LSAss id $ simplifyExp exp
simplifyStmt (SExp exp) = LSExp $ simplifyExp exp

simplifyExp :: Exp -> LightExp
simplifyExp (ExpAdd exp1 exp2) = LExp (COp Add) (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpSub exp1 exp2) = LExp (NCop Sub) (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpMul exp1 exp2) = LExp (COp Mul) (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpDiv exp1 exp2) = LExp (NCop Div) (simplifyExp exp1) (simplifyExp exp2)
simplifyExp (ExpLit int) = LExpLit int
simplifyExp (ExpVar ident) = LExpVar ident

collectVariables :: [Stmt] -> HS.Set String
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