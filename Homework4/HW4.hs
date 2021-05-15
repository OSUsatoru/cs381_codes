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

--Rank = fst element - scd element
rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r | r >= n = rank xs (r-n+m)
              | otherwise = Nothing
                where (n,m) = rankC x

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP x = rank x 0

--(b)
semStatTC :: Prog -> Maybe Stack
semStatTC x | not(r == Nothing) = Just(sem x [])
            | otherwise = Nothing
              where r = rankP x

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i) (s)          = (i:s)
semCmd DUP    (vs@(v:_))   = (v:vs)
semCmd ADD    (v1:v2:vs) = (v1+v2:vs)
semCmd MULT   (v1:v2:vs) = (v1*v2:vs)

sem :: Prog -> Stack -> Stack
sem []     s = s
sem (c:cs) s = sem cs (semCmd c s)

--2