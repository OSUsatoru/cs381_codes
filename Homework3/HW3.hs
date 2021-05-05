-- Exercise 1: A Stack Language (Srikar)





data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show

type Prog = [Cmd]

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D
semCmd (LD i) stack = [i] ++ stack 
semCmd ADD stack = (head(stack) + stack!!1):tail(tail stack)
semCmd MULT stack = (head(stack) * stack!!1):tail(tail stack)
semCmd DUP stack = (head(stack)):stack

sem :: Prog -> D
sem [] stack = stack
sem (x:xs) stack = sem xs (semCmd x stack)

