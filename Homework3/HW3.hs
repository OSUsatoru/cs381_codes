-- Exercise 1: A Stack Language (Srikar)

type Prog = [Cmd]

data Cmd = LD Int
| ADD
| MULT
| DUP

type Stack = [Int]
type D = Stack -> Stack

semCmd :: Cmd -> D

sem :: Prog -> D
