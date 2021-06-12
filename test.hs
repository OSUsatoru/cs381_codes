data Expr = Num Int
          | Name String
          | Let Decls Expr
            deriving Show

data Decls = Equ Expr Expr
           | And Expr Expr Decls
            deriving Show


equA = Equ (Name "x") (Num 1)
prob = Let (equA) (Name "x")

equB = Equ (Name "x") (Num 2)
equC = Equ (Name "y") (Name "x")
probC = Let (And (equB) (equC)) (Name "y")

probD = Let (Equ (Name "x") (Num 3)) (Let (Equ (Name "y") (Name "x")) Name "x")