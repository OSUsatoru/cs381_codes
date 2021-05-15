Note: This file can be directly loaded into ghci.


EXERCISE 1 [50 Points]

The abstract syntax of the stack language was already given in
the exercise.

> type Prog = [Cmd]
>
> data Cmd = LD Int | ADD | MULT | DUP
>            deriving Show

Given the type for a stack, the domain D has to be defined as a
function domain using the Maybe data type to model error values.

> type Stack = [Int]
>
> type StackE = Maybe Stack
>
> type D = StackE -> StackE

A semantic function can always be written in the following form,
mapping from abstract syntax (here, Cmd) into the semantic domain D.

  semCmd :: Cmd -> D

By substituting the definition for the type D, we can obtain

  semCmd :: Cmd -> StackE -> StackE

and by expanding the definition of StackE finally the form shown
below. The semantic function semCmd maps each operation to a
transformation of states, which are given by elements of type
StackE, that is, stacks or errors. Each transformation maps error
states (represented by Nothing) into error states and returns
valid stacks (represented by Just values) only for non-erroneous
input stacks and for commands that find enough arguments.

Note that the "as" pattern "vs@(v:_)" matches the whole list to
vs and at the same time the first element of the list to v.

> semCmd :: Cmd -> Maybe Stack -> Maybe Stack
> semCmd (LD i) (Just s)          = Just (i:s)
> semCmd DUP    (Just vs@(v:_))   = Just (v:vs)
> semCmd ADD    (Just (v1:v2:vs)) = Just (v1+v2:vs)
> semCmd MULT   (Just (v1:v2:vs)) = Just (v1*v2:vs)
> semCmd _      _                 = Nothing

Finally, the semantics for programs threads a stack, initialized
to [], through each command of the program.

> sem :: Prog -> Maybe Stack -> Maybe Stack
> sem []     s = s
> sem (c:cs) s = sem cs (semCmd c s)

Here are some test cases:

> tst1 = [LD 3, DUP, ADD, DUP, MULT]
> tst2 = []::Prog
> err1 = [LD 3, ADD]
> err2 = [LD 3, MULT]
> err3 = [DUP]
>
> tests  = [tst1,      tst2,    err1,    err2,    err3]
> expect = [Just [36], Just [], Nothing, Nothing, Nothing]
> test = map (\p->sem p (Just [])) tests == expect



EXERCISE 2 [50 Points]

The abstract syntax was given in the exercise as follows. (We use here the
names Cmd', State', and sem' to distinguish them from the types/functions
defined in the earlier exercise so that the file can be loaded into ghci.)

> data Cmd' = Pen Mode
>           | MoveTo Int Int
>           | Seq Cmd' Cmd'
>             deriving Show
>
> data Mode = Up | Down
>             deriving Show

Likewise, the semantic domain was already given.

> type Line   = (Int,Int,Int,Int)
> type Lines  = [Line]
> type State' = (Mode,Int,Int)

The semantic function for commands maps a state into a state and a set of
lines. The pen commands do not create any lines; they only affect the state
of the pen. The meaning of moveto depends on the state of the pen: Only with
a pen down will a line be created. In any case, the new position is remembered
in the state. For a sequence of two commands, first the set of lines and a new
state is determined for the first command, then the effect of the second
command is determined using the result state of the first command as input
state.

> semS :: Cmd' -> State' -> (State',Lines)
> semS (Pen Up)       (_,x,y)    = ((Up,x,y),     [])
> semS (Pen Down)     (_,x,y)    = ((Down,x,y),   [])
> semS (MoveTo x' y') (Up,x,y)   = ((Up,x',y'),   [])
> semS (MoveTo x' y') (Down,x,y) = ((Down,x',y'), [(x,y,x',y')])
> semS (Seq c1 c2)    s          = (s2,           l1++l2)
>                                  where
>                                        (s2,l2) = semS c2 s1
>                                        (s1,l1) = semS c1 s

Finally, the semantics function sem' calls semS with an initial state and
takes from its return value only the second component carrying the list
of drawn lines.

> sem' :: Cmd' -> Lines
> sem' c = snd (semS c (Up,0,0))
