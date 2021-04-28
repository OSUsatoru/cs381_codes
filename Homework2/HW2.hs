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

--2b (Satoru, Srikar, Alex, Lance)
accept :: RegEx -> String -> Bool
accept Empty w = w == ""
accept Dot w = (length w) == 1
accept (C x) w = [x] == w
accept (Question e1) s = accept Empty s || accept e1 s

accept (Star e1) [] = True
accept (Star e1) (x:xs) = (accept e1 (x:xs)) || (accept (Plus e1) xs)

accept (Plus e1) [] = False
accept (Plus e1) (x:xs) = (accept e1 (x:xs)) || (accept (Plus e1) xs)

accept (Seq2 (Question e1) (Question e2)) [] = True
accept (Seq2 e1 e2) s = or [accept e1 v && accept e2 w | (v,w) <- splits s]
accept (Or e1 e2) w = (accept e1 w) || (accept e2 w)
accept (Group x) s = accept x s


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
commaSep = Seq2 (Star (catorbatComma)) (catorbat)

catorbat = Seq2 (Or (C 'c') (C 'b')) (Seq2 (C 'a') (C 't'))
catorbatComma = Seq2 (Or (C 'c') (C 'b')) (Seq2 (C 'a') (Seq2 (C 't') (C ',')))
