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
initial' = myTestGen exT exResult

myTestGen :: Expr -> Sigma -> Assertion
myTestGen e s = result @?= s
  where result = doAll e

exT :: Expr
exT = App (abs' "x" (Ref "x")) (abs' "y" (Ref "y"))

exId :: Expr
exId = App (abs' "x" (Ref "x")) (LInt 2)

exResult :: Sigma
exResult = ( Abs (Bind "y" (Ref "y"))
            , empty
            , fromList [(1,Continue Mt),(2,Clo (Bind "y" (Ref "y")) empty)]
            , Mt
            )
