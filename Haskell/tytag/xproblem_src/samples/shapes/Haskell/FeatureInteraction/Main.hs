{-

We slightly instrument the OpenDatatype version. The example touches
upon some feature interaction between multiparameter type classes,
overlapping instances and existential quantification. The code has
been tested with GHC 6.10.4, and the current behavior of GHC has been
like this for a while now.

See this discussion on the GHC mailing list for some background:

http://www.haskell.org/pipermail/glasgow-haskell-bugs/2006-July/005712.html

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}


-- Some kinds of shape as different datatypes

data Rectangle = Rectangle Int Int Int Int
data Circle = Circle Int Int Int


-- A type bound to explicitly collect all kinds of shape

class Shape x
 where
  draw :: x -> IO ()

instance Shape Rectangle
 where
  draw _ = putStrLn "Drawing a rectangle."

instance Shape Circle
 where
  draw _ = putStrLn "Drawing a circle."

instance Shape x
 where
  draw _ = putStrLn "Not sure what I am drawing."


-- An envelope for shapes

data AnyShape = forall x. Shape x => AnyShape x


-- Intersection with overlapping instances

class (Shape x, Shape y) => Intersect x y
 where
  intersect :: x -> y -> String


-- Generic instances for intersection

instance (Shape x, Shape y) => Intersect x y
  where
    intersect s1 s2 = "(Shape x, Shape y) => Intersect x y"


-- Intersection of any shape with a rectangle

instance Shape x => Intersect x Rectangle
 where
  intersect x (Rectangle x1 x2 y1 y2) = "Shape x => Intersect x Rectangle (or v.v.)"


-- A variation on intersect with one opaque shape

intersect' :: Shape x => x -> AnyShape -> String
intersect' x (AnyShape y) = intersect x y

{-

-- Intersection of a rectangle with any shape
-- This instance is only accepted by type checking with -XIncoherentInstances.

instance Intersect x Rectangle => Intersect Rectangle x
 where
  intersect r x = intersect x r

-}

-- Intersection of two rectangles

instance Intersect Rectangle Rectangle
 where
  intersect (Rectangle x1 x2 y1 y2) (Rectangle a1 a2 b1 b2) = "Intersect Rectangle Rectangle"


-- Intersection of two circles

instance Intersect Circle Circle
 where
  intersect (Circle x y r) (Circle x2 y2 r2) = "Intersect Circle Circle"


-- Intersection of a circle and a rectangle

instance Intersect Circle Rectangle
 where
  intersect (Circle x y r) (Rectangle x1 x2 y1 y2) = "Intersect Circle Rectangle (or v.v.)"

instance Intersect Rectangle Circle 
 where
  intersect r c = intersect c r


{-

-- Boolean-based code would not reveal instance selection.

intersectMany :: [AnyShape] -> Bool
intersectMany []      = False
intersectMany (_:[])  = False
intersectMany ((AnyShape x):(AnyShape y):z) =
        intersect x y
     || intersectMany (AnyShape x:z)
     || intersectMany (AnyShape y:z)

-}

-- Intersection for a list of shapes

intersectMany :: [AnyShape] -> String
intersectMany [] = ""
intersectMany (x:[]) = ""
intersectMany ((AnyShape x):(AnyShape y):z) =
    intersect x y
 ++ if null z 
      then ""
      else 
        ", "
        ++ intersectMany (AnyShape x:z)
        ++ ", "
        ++ intersectMany (AnyShape y:z)


-- Test cases

r1 = Rectangle 1 2 3 4
r2 = Rectangle 5 6 7 8
c1 = Circle 1 2 3
c2 = Circle 3 4 5
shapes = [AnyShape r1, AnyShape r2, AnyShape c1, AnyShape c2]

test1 = intersect r1 r2 -- "Intersect Rectangle Rectangle"
test2 = intersect r1 c1 -- "Intersect Circle Rectangle (or v.v.)"
test3 = intersect c1 c2 -- "Intersect Circle Circle"
test4 = intersectMany shapes                    -- uses always the generic instance
test5 = mapM_ (\(AnyShape s) -> draw s) shapes -- draws with the precise instance of Shape


-- Line up tests

main = 
 do
    print $ test1
    print $ test2
    print $ test3
    print $ test4
    test5
