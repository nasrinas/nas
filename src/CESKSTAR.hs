module CESKSTAR where

import qualified Data.Map as M


type Var = String

data Lambd =  LE Var Expr deriving Show

data Expr = Ref Var
          | App Expr Expr
          | Abs Lambd
        deriving (Show)

type Sigma = (Expr,Env,Store,Kont)
type Env = M.Map Var Address

 -- The Kont component of the machine is replaced by a pointer to a continuation
 -- allocated in the store.
data Kont  = Mt
           | AppL Expr Env Address
           | AppR Lambd Env Address
         deriving (Show)


data Storable = Clo Lambd Env
              | Continue Kont
            deriving (Show)

-- The store now maps to denotable values and continuations:
type Store = M.Map Address Storable

type Address = Int

ceskS :: Sigma -> Sigma
ceskS (Ref x,p,s,k)
  = (Abs lam,p',s,k)
    where
      Clo lam p' = s M.! (p M.! x)

ceskS ((App e1 e2),p,s,k)
  = (e1,p,M.insert addr' (Continue k) s ,AppL e2 p addr')
    where
      addr' = label s

ceskS ((Abs l),p,s,AppL e p' addr')
  = (e,p',s,AppR l p addr')

ceskS ((Abs l),p,s,AppR (LE x e) p' a)
  = (e,M.insert x addr' p',M.insert addr' (Clo l p) s,k)
    where
      addr'      = label s
      Continue k = s M.! a

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
--  (Abs (LE "y" (Ref "y")),fromList [],fromList [(1,Continue Mt),(2,Clo (LE "y" (Ref "y"),fromList []))],Mt)
ex :: Expr
ex = App (abs' "x" (Ref "x")) (abs' "y" (Ref "y"))

-- Smart constructor
abs' :: Var -> Expr -> Expr
abs' x e = Abs (LE x e)

-- tttttttttt
