-- Exercise 1: A Stack Language (Srikar, Satoru)





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
semCmd ADD stack = (head(evalS stack) + (evalS stack)!!1):tail(tail (evalS stack))
semCmd MULT stack = (head(evalS stack) * (evalS stack)!!1):tail(tail (evalS stack))
semCmd DUP stack = (head(evalDUP stack)):(evalDUP stack)

sem :: Prog -> D
sem [] stack = stack
sem (x:xs) stack = sem xs (semCmd x stack)

stackLength :: [Int] -> Int
stackLength [] = 0
stackLength (x:xs) = 1 + stackLength xs

evalS :: Stack -> Stack
evalS x = if (stackLength x) > 1 then x else error("There are less than 2 elements in the stack.")

evalDUP :: Stack -> Stack
evalDUP x = if (stackLength x) > 0 then x else error("There are no elements in the stack to be duplicated.")

run :: Prog -> Stack
run p = sem p []

