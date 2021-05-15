import Prelude hiding(Right)

data Expr = Zero
            | Succ Expr
            | Sum [Expr]
            | IfPos Expr Expr

sem :: Expr -> Int
sem Zero = 0
sem (Succ x) = 1 + sem x
sem (Sum []) = 0
sem (Sum (e:es)) = sem e + sem (Sum es)
sem (IfPos e1 e2) | sem e1 > 0 = sem e1
                  | otherwise = sem e2

t= (Succ Zero)
t1 = Succ(Succ Zero)
t2 = Sum [t1,t,t]

type Pos = (Int,Int)

data Move = JumpTo Pos
        | UpBy Int
        | Right
        | Seq Move Move

sem1 :: Move -> Pos -> Pos
sem1 (JumpTo p) _ = p
sem1 (UpBy i) (x,y) = (x,y+i)
sem1 Right (x,y) = (x+1,y)
sem1 (Seq e1 e2) (x,y) = sem1 e2 (sem1 e1 (x,y))

s = UpBy 3