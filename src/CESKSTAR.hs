module CESKSTAR where


import qualified Data.Map as M
import Debug.Trace (trace)

type Addr  = Int
type Var   = String
type Sigma = (Expr,Env,Store,Kont)
type Env   = M.Map Var Addr
type Store = M.Map Addr Storable

data Storable = Clo Lambd Env
              | VInt Int
              | VBool Bool
              | Continue Kont
            deriving (Eq, Show)

data Lambd    = Bind Var Expr deriving (Eq, Show)

data Kont     = Mt
              | AppL Expr Env Addr
              | AppR Lambd Env Addr
              | BinOpR Opr Expr Env Addr
              | BinOpL Opr Expr Env Addr
              | IfK Expr Expr Env Addr
--              | LetBind Lambd Env Addr
            deriving (Eq, Show)

data Expr     = Ref Var
              | App Expr Expr
              | Abs Lambd
              | LInt Int
              | LBool Bool
              | BinOp Opr Expr Expr
              | If Expr Expr Expr
              | Fix Var Expr
--              | Letrec Lambd Expr
            deriving (Eq, Show)

data Opr      = Add | Sub | Mul | Leq deriving (Eq, Show)
-- | Functions for the next available binding
label :: Store -> Addr
label s = (foldl max 0 (M.keys s)) + 1

-- | time-stamped CESK* transition
tst :: Sigma -> Sigma
tst (Ref x,p,s,k)
  = case s M.!(p M.!x) of
      VInt i       -> (LInt i,p,s,k)
      VBool b      -> (LBool b,p,s,k)
      Clo lambd p' -> (Abs lambd,p',s,k)

tst (If f t e,p,s,k)
  = (f,p,M.insert addr' (Continue k) s,IfK t e p addr')
    where
      addr' = label s

tst (BinOp Add e1 e2,p,s,k) = (e1,p,s',BinOpL Add e2 p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (v1,p,s,BinOpL Add e2 p' addr') = (e2,p',s,BinOpR Add v1 p' addr')

tst (v2,p,s,BinOpR Add v1 p' a) = (sumFun v1 v2,p',s,k)
  where
    Continue k = s M.! a

tst (BinOp Sub e1 e2,p,s,k) = (e1,p,s',BinOpL Sub e2 p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (v1,p,s,BinOpL Sub e2 p' addr') = (e2,p',s,BinOpR Sub v1 p' addr')

tst (v2,p,s,BinOpR Sub v1 p' a) = (subFun v1 v2,p',s,k)
  where
    Continue k = s M.! a

tst (BinOp Mul e1 e2,p,s,k) = (e1,p,s',BinOpL Mul e2 p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (v1,p,s,BinOpL Mul e2 p' addr') = (e2,p',s,BinOpR Mul v1 p' addr')

tst (v2,p,s,BinOpR Mul v1 p' a) = (mulFun v1 v2,p',s,k)
  where
    Continue k = s M.! a

tst (BinOp Leq e1 e2,p,s,k) = (e1,p,s',BinOpL Leq e2 p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (v1,p,s,BinOpL Leq e2 p' addr') = (e2,p',s,BinOpR Leq v1 p' addr')

tst (v2,p,s,BinOpR Leq v1 p' a) = (leqFun v1 v2,p',s,k)
  where
    Continue k = s M.! a
-- let rec f x = t  is equal to fix f in (\x.t)
-- <fix f in (\x. t), E, K> --> <t, {f |-> [fix f in (\x. t),E]}::E, K >
tst (Fix f (Abs (Bind x t)),p,s,k) = (t,p',s',k)
  where
    p'    = M.insert f addr' p
    addr' = label (s)
    s'    = M.insert addr' (Clo (Bind x t) p) s
{-
--tst e@(Letrec bi bd,p,s,LetBdy ex p' addr') = trace ("got letrec" ++ show e) (ex,p',s,LetBind bi p addr')
tst (Letrec bi bd,p,s,k) = (bd,p,s',LetBind bi p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (r,p,s,LetBind (Bind x e) p' a) = (e,p'',s'',k)
  where
    p''        = M.insert x addr' p'
    s''        = M.insert addr' (supply r p s) s
    Continue k = s M.! a
    addr'      = label (s)
-}
tst (App e1 e2,p,s,k) = (e1,p,s',AppL e2 p addr')
  where
    addr' = label (s)
    s'    = M.insert addr' (Continue k) s

tst (Abs lambd,p,s,AppL e2 p' addr') = (e2,p',s,AppR lambd p addr')

tst (r,p,s,AppR (Bind x e) p' a) = (e,p'',s'',k)
  where
    p''        = M.insert x addr' p'
    s''        = M.insert addr' (supply r p s) s
    Continue k = s M.! a
    addr'      = label (s)


tst ((LBool False),p,s,IfK t e p' c)
  = (e,p',s,k)
    where
      Continue k = s M.! c

tst ((LBool True),p,s,IfK t e p' c)
  = (t,p',s,k)
    where
      Continue k = s M.! c

supply :: Expr -> Env -> Store -> Storable
supply (Abs lam) p _ = Clo lam p
supply (LInt i) p _  = VInt i
supply (LBool b) p _ = VBool b
supply (Ref x) p  s  = case s M.!(p M.!x) of
                         v  -> v


supply e p _ = error (show e)

sumFun :: Expr -> Expr -> Expr
sumFun (LInt i) (LInt j) = LInt (i+j)

subFun :: Expr -> Expr -> Expr
subFun (LInt i) (LInt j) = LInt (i-j)

mulFun :: Expr -> Expr -> Expr
mulFun (LInt i) (LInt j) = LInt (i*j)

leqFun :: Expr -> Expr -> Expr
leqFun (LInt i) (LInt j) = LBool (i<=j)

initial :: Expr -> Sigma
initial e = (e,M.empty,M.empty,Mt)

final :: Sigma -> Bool
final (Abs _,_,_,Mt)  = True
final (LInt _,_,_,Mt) = True
final (LBool _,_,_,Mt) = True
final _               = False

reduction :: (Sigma -> Sigma) -> (Sigma -> Bool) -> Sigma -> Sigma
reduction g final s'
    | final s'   = s'
    | otherwise  = reduction g final (g s')

doAll :: Expr -> Sigma
doAll e = reduction tst final (initial e)

abs' :: Var -> Expr -> Expr
abs' x e = Abs (Bind x e)
-- Examples:
{-
fact' :: Expr
fact' = Fix "f" (abs' "n" (If (BinOp Leq (Ref "n") (LInt 0))(LInt 1)(BinOp Mul (Ref "n")(App (Ref "f")(BinOp Sub (Ref "n")(LInt 1))))))

factor :: Expr
factor = App fact' (LInt 3)
