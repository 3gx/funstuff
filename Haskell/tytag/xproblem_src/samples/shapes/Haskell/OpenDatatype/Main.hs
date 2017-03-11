{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- Some kinds of shape as different datatypes

data Square    = Square    Int Int Int           deriving (Show)
data Rectangle = Rectangle Int Int Int Int       deriving (Show)
data Circle    = Circle    Int Int Int           deriving (Show)
data Ellipse   = Ellipse   Int Int Int Int       deriving (Show)


-- The set of all shape types

class Shape x
instance Shape Square
instance Shape Rectangle
instance Shape Circle
instance Shape Ellipse


-- The intersection operation

class (Shape x, Shape y) => Intersect x y
 where
  intersect :: x -> y -> Bool


-- Some efficient cases treated specifically

instance Intersect Rectangle Rectangle
 where
  intersect (Rectangle x1 x2 y1 y2) (Rectangle a1 a2 b1 b2) = undefined

instance Intersect Circle Circle
 where
  intersect (Circle x y r) (Circle x2 y2 r2) = undefined

instance Intersect Circle Rectangle
 where
  intersect (Circle x y r) (Rectangle x1 x2 y1 y2) = undefined


-- Case handled by commutativity of operation
  
instance Intersect Rectangle Circle where
  intersect r c = intersect c r


-- Generic defaults

instance Shape s => Intersect Rectangle s where
  intersect r s = undefined

instance Shape s => Intersect s Rectangle where
  intersect s r = undefined

instance (Shape s1, Shape s2) => Intersect s1 s2 where
  intersect s1 s2 = undefined


-- Intersection for all combinations of shapes in a list (nested product)

class  IntersectMany x
 where intersectMany :: x -> Bool

instance IntersectMany ()
  where  intersectMany _ = False

instance Shape x => IntersectMany (x,())
  where  intersectMany _ = False

instance ( Intersect x y
         , IntersectMany (x,z)
         , IntersectMany (y,z)
         ) => IntersectMany (x,(y,z))
 where
  intersectMany (x,(y,z))
   =  intersect x y
   || intersectMany (x,z)
   || intersectMany (y,z)


-- The subset of shapes that are normal(ized)

class Shape s => NormalShape s
instance NormalShape Square
instance NormalShape Circle


-- The normalization operation

class (Shape s1, NormalShape s2)
   => Normalize s1 s2
    | s1 -> s2
 where
  normalize :: s1 -> s2


-- Exhaustive case discrimination

{-

instance Normalize Square Square
 where
  normalize = id

instance Normalize Circle Circle
 where
  normalize = id

instance NormalShape s
      => Normalize s s
 where
  normalize = id

-}

instance ( NormalShape s1
         , Shape s2, s1 ~ s2
         )
           => Normalize s1 s2
 where
  normalize = id

instance Normalize Rectangle Square
 where
  normalize = undefined

instance Normalize Ellipse Circle
 where
  normalize = undefined


-- No testing

main = do print ()
