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
semCmd ADD stack = (head(evalS ADD stack) + (stack)!!1):tail(tail stack)
semCmd MULT stack = (head(evalS MULT stack) * (stack)!!1):tail(tail stack)
semCmd DUP stack = (head(evalD stack)):stack

sem :: Prog -> D
sem [] stack = stack
sem (x:xs) stack = sem xs (semCmd x stack)

stackLength :: [Int] -> Int
stackLength [] = 0
stackLength (x:xs) = 1 + stackLength xs

evalS :: Cmd -> Stack -> Stack
evalS ADD x = case (stackLength x) of
                len -> if len > 1 then x
                       else error("Expression ADD: There are less than 2 elements in the stack.")
evalS MULT x = case (stackLength x) of
                len -> if len > 1 then x
                       else error("Expression MULT: There are less than 2 elements in the stack.")

--evalS x = if (stackLength x) > 1 then x else error("There are less than 2 elements in the stack.")

evalD :: Stack -> Stack
evalD x = case (stackLength x) of
                len -> if len > 0 then x
                       else error("Expression DUP: There are less than 1 elements in the stack.")
--evalD x = if (stackLength x) > 0 then x else error("There are no elements in the stack to be duplicated.")

run :: Prog -> Stack
run p = sem p []

-- Exercise 2: Mini Logo (Lance)

data Cmd' = Pen Mode
          | MoveTo Int Int
          | Seq Cmd' Cmd'
            deriving Show

data Mode   = Up | Down
              deriving Show

type State  = (Mode,Int,Int)

type Line   = (Int,Int,Int,Int)
type Lines  = [Line]

semS :: Cmd' -> State -> (State,Lines)
semS (Pen Up)         (_,x,y)     = ((Up,x,y),[])
semS (Pen Down)       (_,x,y)     = ((Down,x,y),[])
semS (MoveTo a b)     (Up,_,_)    = ((Up,a,b),[])
semS (MoveTo a b)     (Down,x,y)  = ((Down,a,b),[(x,y,a,b)])
semS (Seq cmd1 cmd2)  state1      = (state3, lines1 ++ lines2)
                                    where (state2, lines1) = semS cmd1 state1
                                          (state3, lines2) = semS cmd2 state2

sem' :: Cmd' -> Lines
sem' cmd = snd (semS cmd (Up, 0, 0))

