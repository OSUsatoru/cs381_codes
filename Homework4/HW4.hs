-- Exercise 1. A Rank-Based Type System for the Stack Language (Satoru, Lance)

import Data.List
import Data.Ord


type Prog = [Cmd]
type Stack = [Int]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEC
         | SWAP
         | POP Int
         deriving Show

type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC (ADD) = (2,1)
rankC (MULT) = (2,1)
rankC (DUP) = (1,2)
rankC (DEC) = (1,1)
rankC (SWAP) = (2,2)
rankC (POP x) = (x,0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r | r >= n = rank xs (r-n+m)
              | otherwise = Nothing
                where (n,m) = rankC x

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP x = rank x 0

-- semStatTC :: Prog -> Maybe Stack
-- semStatTC x | rankP x /= Nothing = Just(sem x [])
--             | otherwise = Nothing 

-- rankP does error checking already so sem's
-- type and definition can be simplified like so:

-- type D = Stack -> Stack

-- sem :: Prog -> D
-- sem []     s = s
-- sem (c:cs) s = sem cs (semCmd c s)

-- semCmd :: Cmd -> D
-- semCmd (LD x)   s           = (x:s)
-- semCmd DUP     (vs@(v:_))   = (v:vs)
-- semCmd ADD     (v1:v2:vs)   = (v1+v2:vs)
-- semCmd MULT    (v1:v2:vs)   = (v1*v2:vs)
-- semCmd DEC     (v:vs)       = (v-1:vs)
-- semCmd SWAP    (v1:v2:vs)   = (v2:v1:vs)
-- semCmd (POP x)  s           = drop x s

-- Exercise 2. Shape Language (Srikar, Alex)

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

type Pixel = (Int,Int)
type Image = [Pixel]

sem :: Shape -> Image 

sem X = [(1,1)]

sem (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem s2]                  
where d1 = sem s1

sem (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem s1]                  
where d2 = sem s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

bbox :: Shape -> BBox
bbox X = (1,1)
bbox a = maximum(sem(a)) 


maximum :: Image -> BBox
maximum [] a b = (a, b)
maximum ((x,y):xs) a b = maximum xs c d
                         where c = if x > a then x else a
                               d = if y > b then y else b


rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (LR s1 s2) = if (snd bbox(s1)) == (snd bbox(s2)) then (((fst bbox(s1)) + (fst bbox(s2))), (snd bbox(s1)))
rect (TD s1 s2) = if (fst bbox(s1)) == (fst bbox(s2)) then ((fst bbox(s1)), ((snd bbox(s1)) + (snd bbox(s2))))
