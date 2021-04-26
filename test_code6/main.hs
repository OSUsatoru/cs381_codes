import Prelude hiding (fst,length)

data Tuple = Tuple {a::Int, b::Int}

fst::Tuple-> Int
fst(Tuple a _)= a

--main = print $ fst(Tuple 123 456) -- : 123


data List a = Nil | Cons a(List a)

length :: List a -> Int
length Nil         = 0
length(Cons x xs)= 1 + length xs


main = do print $ length Nil                             -- : 0
          print $ length $ Cons "a"(Cons "b" (Cons "c" Nil))  -- : 3

