import Prelude hiding (sum,length)
import Debug.Trace(trace)

sum::[Int]->Int
sum [] = 0
sum (x:xs) = x + sum xs

keepTwos::[Int]->[Int]
keepTwos [] = []
keepTwos (x:xs) | x==2 = 2: keepTwos xs
                | otherwise = keepTwos xs

countTwos xs = length(keepTwos xs)


length :: [a]->Int
length [] = 0
length (_:xs) = 1+length xs

--g::[a]->[a]->[a]
g[] ys = ys
g(x:xs) ys = trace("= g"++show xs++" ("++show x++":"++show ys++")")
             (g xs (x:ys))
--g(x:xs) ys = g xs(x:ys)

{--
main = do
    map succ xs
    map(map succ) ys
    where
        xs = [1,2,3]
        ys = [xs,[7]]
--}