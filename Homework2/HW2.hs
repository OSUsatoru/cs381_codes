-- Exercise 1: Mini Logo

-- 1a (Lance)
data Cmd = Pen Mode
         | Moveto Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Seq Cmd [Cmd]
         deriving Show

data Mode = Up | Down
          deriving Show

data Pos = N Int | S String
          deriving Show

data Pars = P (String, [String])
          deriving Show

data Vals = V (Int, [Int])
          deriving Show

-- 2b (Lance)
vector = Def "vector" (P ("x1", ["x2", "y1", "y2"]))
        (Seq (Pen Up) [Moveto (S "x1") (S "y1"),
                       Pen Down,
                       Moveto (S "x2") (S "y2"),
                       Pen Up])

-- 3c (Lance)
drawStep = Def "drawStep" (P ("x1", ["x2", "y1", "y2"]))
          (Seq (Pen Up) [Moveto (S "x1") (S "y1"),
                         Pen Down,
                         Moveto (S "x2") (S "y1"),
                         Moveto (S "x2") (S "y2"),
                         Pen Up])

steps :: Int -> Cmd
steps 0 = Seq (Pen Up) [Moveto (N 0) (N 0),
                        Pen Down,
                        Pen Up]

steps n = Seq (Pen Up) [Call "drawStep" (V (n, [(n-1), n, (n-1)])), steps (n-1)]


-- Exercise 2: Regular Expressions

-- 2a (Lance)
data RegEx = Empty
           | Dot
           | C Char
           | Question RegEx
           | Star RegEx
           | Plus RegEx
           | Seq2 RegEx RegEx
           | Or RegEx RegEx
           | Group RegEx
           deriving (Eq, Show)

--2b (Satoru, Srikar, Alex)
accept :: RegEx -> String -> Bool
accept Empty w = w == ""
accept Dot w = (length w) == 1
accept (C x) w = [x] == w
accept (Group x) s = accept x s


accept (Plus e1) [] = False
accept (Plus e1) (x:xs) = (accept e1 (x:xs)) || (accept (Plus e1) xs)

accept (Star e1) []
    | e1 == Empty = True
    | otherwise = False
accept (Star e1) (x:xs) = (accept e1 [x]) || (accept (Star e1) xs)

-- accept (Star e1) (x:xs) 
--     | e1 == (C x) = accept (C x) (x:xs)
--     | e1 == Dot = accept Dot [x]
--     | otherwise = accept (Plus e1) xs



-- accept (Star e1) (x:xs) 
--     | (accept e1 x)y
--     | otherwise = (accept e1 xs)

-- accept (Star e1) s = accept Empty s || or [accept e1 v && accept (Star e1) w | (v,w) <- splits s]
accept (Seq2 e1 e2) s = or [accept e1 v && accept e2 w | (v,w) <- splits s]
accept (Or e1 e2) w = (accept e1 w) || (accept e2 w)




splits :: [a] -> [([a],[a])]
splits [] = []
splits [x] = [([],[x]),([x],[])]
splits (x:xs) = [([],x:xs)] ++ [(x:s,t) | (s,t) <- splits xs]


--2c (Satoru, Lance)

classify :: RegEx -> [String] -> IO ()
classify e ws = putStrLn ("ACCEPT:\n"++show acc++"\nREJECT:\n"++show rej)
    where acc = filter (accept e) ws
          rej = filter (not.(accept e)) ws

commaSepTest = ["cat","cat,bat","cat,cat","bat","",",","dog",
                ",cat","cat,","catcat","cat,,bat","cat,bat,"]

commaSep :: RegEx
commaSep = Or (Or cat bat)
              (Or (Or (Seq2 catComma bat) (Seq2 catComma cat))
                  (Or (Seq2 batComma bat) (Seq2 batComma cat)))

cat = Seq2 (C 'c') (Seq2 (C 'a') (Seq2 (C 't') Empty))
catComma = Seq2 (C 'c') (Seq2 (C 'a') (Seq2 (C 't') (Seq2 (C ',') Empty)))
bat = Seq2 (C 'b') (Seq2 (C 'a') (Seq2 (C 't') Empty))
batComma = Seq2 (C 'b') (Seq2 (C 'a') (Seq2 (C 't') (Seq2 (C ',') Empty)))
