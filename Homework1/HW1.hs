import HW1types

-- Exercise 1: Lists (Satoru)

-- (a)
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = (x,1):[]
ins x ((y,n):ys) | x==y = (y,n+1):ys
                 | otherwise = (y,n):ins x ys

-- (b)
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ys) | x==y && n>1 = (y,n-1):ys
                 | x==y && n==1 = ys
                 | otherwise = (y,n):del x ys
-- (c)
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

--(d) a < b
bag_find :: Eq a => a -> Int -> Bag a -> Bool
bag_find x xn [] = False
bag_find x xn ((y,yn):ys) | x==y && xn <= yn = True
                          | x==y && xn > yn = False
                          | otherwise = bag_find x xn ys

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _  = True
subbag ((x,n):xs) y | bag_find x n y = subbag xs y
                    | otherwise = False


-- (e)
isSet :: Eq a => Bag a -> Bool
isSet [] = True
isSet ((x,n):xs) | not(n==1) = False
                 | otherwise = isSet xs

-- (f)
size :: Bag a -> Int
size [] = 0
size ((x,n):xs) = n+size xs



-- Exercise 2: Graphs (Srikar and Alex)
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm([(fst x), (snd x)] ++ nodes(xs))

suc :: Node -> Graph -> [Node]
suc node [] = []
suc node (x:xs)
    | node == (fst x) = [snd x] ++ (suc node xs)
    | otherwise = suc node xs

detach :: Node -> Graph -> Graph
detach _ [] = []
detach n (x:xs)
    | fst x == n || snd x == n = [] ++ detach n xs
    | otherwise = [x] ++ detach n xs

cyc :: Int -> Graph
cyc n
    | (n == 0) = []
    | (n == 1) = [(1,1)]
    | otherwise = zip [1..n-1] [2..n] ++ [(n, 1)]

-- Exercise 3: Data Types (Lance)
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ rad) = rad * 2
width (Rect _ x _) = x

bbox :: Shape -> BBox
bbox (Pt point) = (point,point)
bbox (Circle (x,y) rad) = ((x-rad,y-rad),(x+rad,y+rad))
bbox (Rect (x,y) wid len) = ((x,y),(x+wid,y+len))

minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) rad) = x-rad
minX (Rect (x,y) wid len) = x

move :: Shape -> Point -> Shape
move (Pt (x,y)) (x2,y2) = Pt (x+x2,y+y2)
move (Circle (x,y) rad) (x2,y2) = Circle (x+x2,y+y2) rad
move (Rect (x,y) wid len) (x2,y2) = Rect (x+x2,y+y2) wid len
