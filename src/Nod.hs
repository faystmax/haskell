module Nod(nod)where

nodSingle :: Int -> Int -> Int
nodSingle a 0 = a
nodSingle a b = nodSingle b (rem a b)

nod :: [Int] -> Int
nod [] =  0
nod (h:[]) =  h
nod (h:t) = nod(nodSingle h  (head t) : tail(t))

-- module Main  where
--
-- import Test.Tasty
-- import Test.Tasty.HUnit
--
-- import Nod
--
-- main :: IO ()
-- main = do
--   defaultMain (testGroup "Nod Tests" [nodTest10, nodTest1])
--
-- nodTest10 :: TestTree
-- nodTest10 = testCase "Testing nodTest5"
--   (assertEqual "nod[20,10,30,50] = 10" 10 (nod [20,10,30,50]))
--
-- nodTest1 :: TestTree
-- nodTest1 = testCase "Testing add5"
--   (assertEqual "nod[18,12,25,1] = 1" 1 (nod [18,12,25,1]))
