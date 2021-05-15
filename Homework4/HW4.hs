type Prog = [Cmd]
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

