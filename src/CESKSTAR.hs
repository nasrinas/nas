module CESKSTAR where

import qualified Data.Map as M

type Address = Int
type Var = String
data Opr = Add | Sub | Mul | Leq deriving (Eq, Show)
data Lambd =  Bind Var Expr deriving Show
type Sigma = (Expr,Env,Store,Kont)
type Env = M.Map Var Address

-- The store now maps to denotable values and continuations:
type Store = M.Map Address Storable

data Expr = Ref Var
          | Abs Lambd
          | App Expr Expr
          | BinOpr Opr Expr Expr
          | If Expr Expr Expr
          | LInt Int
          | LBool Bool
        deriving Show


 -- The Kont component of the machine is replaced by a pointer to a continuation
 -- allocated in the store.
data Kont  = Mt
           | AppL Expr Env Address
           | AppR Lambd Env Address
           | BinOprL Opr Expr Env Address
           | BinOprR Opr Expr Env Address
           | IfK Expr Expr Env Address
         deriving Show


data Storable = VInt Int
              | VBool Bool
              | Clo Lambd Env
              | Continue Kont
            deriving (Show)


ceskS :: Sigma -> Sigma
ceskS (Ref x,p,s,k)
  = case s M.! (p M.! x) of
      VInt i     -> (LInt i,p,s,k)
      VBool b    -> (LBool b,p,s,k)
      Clo lam p' -> (Abs lam,p',s,k)
      _          -> error ("unknown")

ceskS (If f t e,p,s,k)
  = (f,p,M.insert addr' (Continue k) s,IfK t e p addr')
    where
      addr' = label s

ceskS ((App e1 e2),p,s,k)
  = (e1,p,M.insert addr' (Continue k) s ,AppL e2 p addr')
    where
      addr' = label s

ceskS ((Abs l),p,s,AppL e p' addr')
  = (e,p',s,AppR l p addr')

ceskS ((Abs l),p,s,AppR (Bind x e) p' a)
  = (e,M.insert x addr' p',M.insert addr' (Clo l p) s,k)
    where
      addr'      = label s
      Continue k = s M.! a

ceskS (ite,p,s,IfK t e p' addr')
  = case ite of
      LBool True  -> (t,p',s,k)
      LBool False -> (e,p',s,k)
  where
    Continue k = s M.! addr'

label :: Store -> Address
label s = (foldl max 0 (M.keys s)) + 1

-- The inital state is as same as the one in CESK
initial :: Expr -> Sigma
initial e = (e,M.empty,M.empty,Mt)

-- The evaluation part is as same as the CESK
final :: Sigma -> Bool
final (Abs _,_,_,Mt) = True
final _              = False

reduction :: (Sigma -> Sigma) -> (Sigma -> Bool) -> Sigma -> Sigma
reduction g final s'
    | final s'   = s'
    | otherwise  = reduction g final (g s')

doAll :: Expr -> Sigma
doAll e = reduction ceskS final (initial e)

-- |  >>> doAll ex
--  (Abs (Bind "y" (Ref "y")),fromList [],fromList [(1,Continue Mt),(2,Clo (Bind "y" (Ref "y"),fromList []))],Mt)
ex :: Expr
ex = App (abs' "x" (Ref "x")) (abs' "y" (Ref "y"))

-- Smart constructor
abs' :: Var -> Expr -> Expr
abs' x e = Abs (Bind x e)
