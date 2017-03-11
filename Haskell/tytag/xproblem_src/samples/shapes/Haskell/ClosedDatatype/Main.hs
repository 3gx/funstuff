-- Some kinds of shape

data Shape = Square Int Int Int
           | Rectangle Int Int Int Int
           | Circle Int Int Int
           | Ellipse Int Int Int Int

-- The intersection operation

intersect :: Shape -> Shape -> Bool

-- Some efficient cases treated specifically

intersect (Rectangle x1 x2 y1 y2) (Rectangle a1 a2 b1 b2) = undefined
intersect (Circle x y r)          (Circle x2 y2 r2)       = undefined
intersect (Circle x y r)          (Rectangle x1 x2 y1 y2) = undefined

-- Case handled by commutativity of operation

intersect r@(Rectangle _ _ _ _) c@(Circle _ _ _) = intersect c r

-- A default cast

intersect s1 s2 = undefined


-- Intersection for all combinations of shapes in a list

intersectMany :: [Shape] -> Bool
intersectMany []      = False
intersectMany (x:[])  = False
intersectMany (x:y:z) =  intersect x y
                      || intersectMany (x:z)
                      || intersectMany (y:z)


-- No testing

main = do print ()
