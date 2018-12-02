module Nod(nod)where

nodSingle :: Int -> Int -> Int
nodSingle a 0 = a
nodSingle a b = nodSingle b (rem a b)

nod :: [Int] -> Int
nod [] =  0
nod (h:[]) =  h
nod (h:t) = nod(nodSingle h  (head t) : tail(t))

