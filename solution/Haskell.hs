module Haskell where

import HW1types


--
-- Exercise 1
--

-- (a)
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,n):ms) | x==y      = (y,n+1):ms
                 | otherwise = (y,n):ins x ms

-- (b)
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ms) | x==y && n>1 = (y,n-1):ms
                 | x==y        = ms
                 | otherwise   = (y,n):del x ms

-- (c)
bag :: Eq a => [a] -> Bag a
bag = foldr ins []

-- (d)
member :: Eq a => (a,Int) -> Bag a -> Bool
member _    [] = False
member (x,n) ((y,m):ys) | x==y && n<=m = True
                        | otherwise    = member (x,n) ys

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag []     _  = True
subbag (x:xs) ys = member x ys && subbag xs ys
-- subbag ((x,n):xs) ys = case lookup x ys of
--                          Just m  -> n<=m && subbag xs ys
--                          Nothing -> False

-- (e)
isSet :: Eq a => Bag a -> Bool
isSet []         = True
isSet ((x,1):xs) = isSet xs
isSet _          = False

-- (f)
size :: Bag a -> Int
size = sum . map snd

xs,ys :: [Int]
xs = reverse [5,7,2,3,7,8,3,7]
ys = reverse [5,5,7,8,3,8,7]

lx = bag xs
ly = bag ys
lz = del 8 ly
la = del 5 lz
lb = del 3 la


--
-- Exercise 2
--

-- (a)
nodes :: Graph -> [Node]
nodes g = norm (map fst g++map snd g)
--
-- nodes g = norm (concat [[v,w] | (v,w) <- g])

-- (b)
suc :: Node -> Graph -> [Node]
suc v g = norm [w | (u,w) <- g, v==u]

-- (c)
detach :: Node -> Graph -> Graph
detach v g = [(u,w) | (u,w) <- g, v/=u, v/=w]

-- (d)
cyc :: Int -> Graph
cyc 0 = []
cyc n = [(v,v+1) | v <- [1..n-1]] ++ [(n,1)]
--
-- cyc n = zip [1..n] ([2..n]++[1])

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

{- | Tests

>>> nodes g
[1,2,3,4]

>>> suc 2 g
[3,4]

>>> suc 4 g
[]

>>> suc 4 h
[4]

>>> detach 3 g
[(1,2),(2,4)]

>>> detach 2 h
[(1,3),(4,4)]

>>> cyc 4
[(1,2),(2,3),(3,4),(4,1)]

-}


--
-- Exercise 3
--

-- (a)
width :: Shape -> Length
width (Circle _ r) = 2*r
width (Rect _ w _) = w
width _            = 0

-- (b)
bbox :: Shape -> BBox
bbox (Circle (x,y) r) = ((x-r,y-r),(x+r,y+r))
bbox (Rect (x,y) w h) = ((x,y),(x+w,y+h))
bbox (Pt p)           = (p,p)

-- (c)
minX :: Shape -> Number
-- minX s = x where (x,_):_ = bbox s
-- minX s = fst (fst (bbox s))
minX = fst . fst . bbox

-- (d)
addPoint :: Point -> Point -> Point
addPoint (x,y) (dx,dy) = (x+dx,y+dy)

(+.) = addPoint

move :: Shape -> Point -> Shape
move (Circle p r) v = Circle (p +. v) r
move (Rect p w h) v = Rect   (p +. v) w h
move (Pt p)       v = Pt     (p +. v)


f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
