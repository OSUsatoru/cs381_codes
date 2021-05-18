-- Exercise 1. A Rank-Based Type System for the Stack Language (Satoru, Lance)

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

-- Exercise 2. Shape Language (Srikar, Lance, Alex)

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD a b) | x1 >= x2 = (x1, y1 + y2)
              | otherwise = (x2, y1 + y2) 
              where (x1, y1) = bbox a
                    (x2, y2) = bbox b

bbox (LR a b) | y1 >= y2 = (x1 + x2, y1)
              | otherwise = (x1 + x2, y2) 
              where (x1, y1) = bbox a
                    (x2, y2) = bbox b

rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD a b) | rect a /= Nothing && rect b /= Nothing && x1 == x2 = Just (x1, y1 + y2)
              | otherwise = Nothing
              where (x1, y1) = bbox a
                    (x2, y2) = bbox b

rect (LR a b) | rect a /= Nothing && rect b /= Nothing && y1 == y2 = Just (x1 + x2, y1)
              | otherwise = Nothing
              where (x1, y1) = bbox a
                    (x2, y2) = bbox b            
