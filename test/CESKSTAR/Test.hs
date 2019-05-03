module CESKSTAR.Test where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Data.Map

import CESKSTAR


tests :: TestTree
tests = testGroup "Tests" [unitTests]

--properties :: TestTree
--properties = testGroup "Properties" [scProps, qcProps]

{-scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]
-}
unitTests = testGroup "Unit tests"
  [ --testCase "List comparison (different length)" $
--      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
--  , testCase "List comparison (same length)" $
--      [1, 2, 3] `compare` [1,2,2] @?= LT
--  , testCase "An example" myTest
  testCase "basic application" initial'

  ]

initial' :: Assertion
initial' = myTestGen ex' exResult

myTestGen :: Expr -> Sigma -> Assertion
myTestGen e s = result @?= s
  where result = doAll e


ex' :: Expr
ex' = App (abs' "x" (Ref "x")) (abs' "y" (Ref "y"))

intId :: Expr
intId = App (abs' "x" (Ref "x")) (LInt 2)

boolId :: Expr
boolId = App (abs' "x" (Ref "x")) (LBool False)

condIf :: Expr
condIf = App (abs' "x" (If (Ref "x") (LInt 2) (LInt 3))) (LBool True)

addMul :: Expr
addMul = App  (abs' "x" (If (Ref "x") (BinOpr Add (LInt 1) (LInt 3)) (BinOpr Mul (LInt 5)(LInt 6)))) (LBool False)

leqIf :: Expr
leqIf = App (abs' "x" (If (BinOpr Leq (Ref "x") (LInt 2)) (LInt 0) (LInt 1))) (LInt 9)

exId :: Expr
exId = abs' "x" (Ref "x")

exnotB :: Expr
exnotB = abs' "x" (If (Ref "x") (LBool False) (LBool True))

exNot :: Expr
exNot = App (App exId exnotB) (LBool True)

{-exF :: Expr
exF = Letrec (Bind "f" (Ref "f")) (abs' "n" (If (BinOp Leq (Ref "n")(LInt 0))(LInt 1)(BinOp Mul (Ref "n")(App (Ref "fact")(BinOp Sub (Ref "n")(LInt 1))))))

factor :: Expr
factor = App exF (LInt 3)
-}

exResult :: Sigma
exResult = ( Abs (Bind "y" (Ref "y"))
            , fromList []
            , fromList [(1,Continue Mt),(2,Clo (Bind "y" (Ref "y")) (fromList []))]
            , Mt
            )
{-
intIdResult :: Sigma
intIdResult = ( LInt 2
               , fromList [("x",2)]
               , fromList [(1,Continue Mt),(2,VInt 2)]
               , Mt
               )

boolIdResult :: Sigma
boolIdResult = ( LBool False
                , fromList [("x",2)]
                , fromList [(1,Continue Mt),(2,VBool False)]
                , Mt
                )

condIfResult :: Sigma
condIfResult = ( LInt 2
                , fromList [("x",2)]
                , fromList [(1,Continue Mt),(2,VBool True),(3,Continue Mt)]
                , Mt
                )

addMulResult :: Sigma
addMulResult = ( LInt 30
                , fromList [("x",2)]
                , fromList [(1,Continue Mt),(2,VBool False),(3,Continue Mt),(4,Continue Mt)]
                , Mt
                )

leqIfResult :: Sigma
leqIfResult = ( LInt 1
               , fromList [("x",2)]
               , fromList [(1,Continue Mt),(2,VInt 9),(3,Continue Mt),(4,Continue (IfK (LInt 0) (LInt 1) (fromList [("x",2)]) 3))]
               , Mt
               )

exNotResult :: Sigma
exNotResult = ( LBool False
               , fromList [("x",4)]
               , fromList [(1,Continue Mt),(2,Continue (AppL (LBool True) (fromList []) 1)),(3,Clo (Bind "x" (If (Ref "x") (LBool False) (LBool True))) (fromList [])),(4,VBool True),(5,Continue Mt)]
               , Mt
               )
-}
