import Prelude hiding (length)
test :: Int -> Int
test x = if 5>6 then
    5
    else
    6

length:: [a] -> Int
length list | not (null list) = length (tail list) + 1
            | otherwise = 0

double::Int->Int
double x = x*2

fourth = double.double

range :: Int -> [ Int ]
range 0 = []
range x = [x]++range(x-1)

--g::Int->[Int]->[Int]
g _ [] = []
g y (x:xs) | y>0       = g (y-1) xs
           | otherwise = xs

nats::[Int]
nats = 1:map succ nats
--nats = 1:map (+2) nats

fibs = 1: 1 : zipWith (+) fibs (tail fibs)

main = do
    let xs = [[1],[2],[3]]
    print $ map head xs
    print $ sum [1..100]
    print $ foldr (+) 0 [1..100]

{-
take 9 nats
zip [1..10] ['a'..'j']
zipWith (+) [1..10] [2..5]
-}
