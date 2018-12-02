module Main  where

import Test.Tasty
import Test.Tasty.HUnit

import Nod

main :: IO ()
main = do
  defaultMain (testGroup "Nod Tests" [nodTest10, nodTest1, nodTest25])

nodTest10 :: TestTree
nodTest10 = testCase "Testing nodTest5"
  (assertEqual "nod[20,10,30,50] = 10" 10 (nod [20,10,30,50]))

nodTest1 :: TestTree
nodTest1 = testCase "Testing nodTest1"
  (assertEqual "nod[18,12,25,1] = 1" 1 (nod [18,12,25,1]))

nodTest25 :: TestTree
nodTest25 = testCase "Testing nodTest25"
  (assertEqual "nod[100,25,250,1000] = 25" 25 (nod [100,25,250,1000]))
