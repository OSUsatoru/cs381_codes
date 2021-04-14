import HW1types

-- Exercise 1: Lists




-- Exercise 2: Graphs
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm([(fst x), (snd x)] ++ nodes(xs))

suc :: Node -> Graph -> [Node]
suc node [] = []
suc node (x:xs)
    | node == (fst x) = [snd x] ++ (suc node xs)
    | otherwise = suc val xs

cyc :: Int -> Graph



-- Exercise 3: Data Types
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
