module HW1types where

import Data.List (nub,sort)


-- Types for Exercise 1
--
type Bag a = [(a,Int)]
-- (a)
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = (x,1):[]
ins x ((y,n):ys) | x==y = (y,n+1):ys
                 | otherwise = (y,n):ins x ys

-- (b)
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ys) | x==y && n>1 = (y,n-1):ys
                 | x==y && n==1 = ys
                 | otherwise = (y,n):del x ys
-- (c)
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

--(d) a < b
bag_find :: Eq a => a -> Int -> Bag a -> Bool
bag_find x xn [] = False
bag_find x xn ((y,yn):ys) | x==y && xn <= yn = True
                          | x==y && xn > yn = False
                          | otherwise = bag_find x xn ys

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _  = False
subbag ((x,n):xs) y | bag_find x n y = True
                    | otherwise = subbag xs y

-- (e)
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x,n):xs) | not(n==1) = False
                 | otherwise = isSet xs

-- (f)
size :: Bag a -> Int
size [] = 0
size (_:xs) = 1+size xs

-- Types and functions for Exercise 2
--
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub


-- Types for Exercise 3
--
type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)
