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

--(a)
type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD x) = (0,1)
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

--(b)
-- semStatTC :: Prog -> Maybe Stack
-- semStatTC x | rankP x /= Nothing = Just(sem x [])
--             | otherwise = Nothing 

-- rankP does error checking already so sem's
-- type and definition can be simplified like so:

-- sem :: Prog -> Stack -> Stack
-- sem []     s = s
-- sem (c:cs) s = sem cs (semCmd c s)

-- semCmd :: Cmd -> Stack -> Stack
-- semCmd (LD x)   s           = (x:s)
-- semCmd DUP     (vs@(v:_))   = (v:vs)
-- semCmd ADD     (v1:v2:vs)   = (v1+v2:vs)
-- semCmd MULT    (v1:v2:vs)   = (v1*v2:vs)
-- semCmd DEC     (v:vs)       = (v-1:vs)
-- semCmd SWAP    (v1:v2:vs)   = (v2:v1:vs)
-- semCmd (POP x)  s           = drop x s

sem :: Prog -> Maybe Stack -> Maybe Stack
sem []     s = s
sem (c:cs) s = sem cs (semCmd c s)

semCmd :: Cmd -> Maybe Stack -> Maybe Stack
semCmd (LD x)  (Just s)          = Just (x:s)
semCmd DUP     (Just vs@(v:_))   = Just (v:vs)
semCmd ADD     (Just (v1:v2:vs)) = Just (v1+v2:vs)
semCmd MULT    (Just (v1:v2:vs)) = Just (v1*v2:vs)
semCmd DEC     (Just (v:vs))     = Just (v-1:vs)
semCmd SWAP    (Just (v1:v2:vs)) = Just (v2:v1:vs)
semCmd (POP x) (Just s)          = Just (drop x s)
semCmd _       _                 = Nothing

-- Exercise 2. Shape Language
