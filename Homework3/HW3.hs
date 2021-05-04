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
semCmd ADD stack = (stack!!0 + stack!!1):tail(tail stack)
semCmd MULT stack = (stack!!0 * stack!!1):tail(tail stack)
semCmd DUP stack = (stack!!0):stack

sem :: Prog -> D
sem (x:xs) stack = semCmd x stackUpdated
                   where stackUpdated = sem xs stack

