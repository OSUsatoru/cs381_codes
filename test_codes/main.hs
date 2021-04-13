import Prelude hiding(even,head,tail,nth)

fac :: Int -> Int
--fac n = if n<2 then 1 else n*fac(n-1)
--fac n | n<2 = 1
--      | True = n*fac(n-1)
fac 1 = 1
fac n = n*fac(n-1)

fib :: Int -> Int
fib 0=0
fib 1=1
fib n = fib(n-1)+fib(n-2)

even :: Int -> Bool
even 0 = True
even 1 = False
even n = even(n-2)

head :: [a] -> a
head(x:xs) = x

tail :: [a] -> [a]
tail(_:xs) = xs

nth :: Int -> [a] -> a
nth 0 (x:_) = x
nth n (_:xs) = nth(n-1) xs

main = do
    print $ [1..4] ++ [5]

--check2 :: Int -> Int
--check2 n = (n'quot'2)