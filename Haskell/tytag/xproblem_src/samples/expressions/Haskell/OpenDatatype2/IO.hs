-- Input/Output (aka De-/serialization) for expressions

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module IO where

import Data.Generics
import Data.Tree
import DataBase
import DataExtension


{-

Let's serialize.  In fact, let's "treealize".  It should be noted that
such conversion, expressiveness-wise, is like printing, which we
covered before. Hence, let's use SYB to provides that operation in a
generic manner.

-}

toTree :: Data x => x -> Tree String
toTree x = Node
               (showConstr (toConstr x))
               (gmapQ toTree x)


{-

We could try to use the magic of SYB to do the de-serialization.  In
fact, this is not straigthforward. (Solutions are appreciated, and
should be sent directly to haskell-cafe.) Let us try to understand
de-serialization (or de-treealization) as an "open" operation here. So
let us apply the same class-based method as in the case of printing
and evaluation; this approach fails, as we discuss below.

-}


class FromTree x
 where
  fromTree :: Tree String -> x

instance FromTree Lit
 where
  fromTree (Node "Lit" [i]) = Lit (fromTree i)

instance (Exp e, Exp e', FromTree e, FromTree e') => FromTree (Add e e')
 where
  fromTree (Node "Add" [x,y]) = Add (fromTree x) (fromTree y)

instance (Exp e, FromTree e) => FromTree (Neg e)
 where
  fromTree (Node "Neg" [x]) = Neg (fromTree x)

instance FromTree Int
 where
  fromTree (Node s []) = read s


{-

The problem with this apparently open and extensible solution is that
we would need to know the precise type of the result of
de-serialization for it to work on the grounds of the many instances
shown. This is absolutely unrealistic. (Types represent shape of input
in our model.) We need a way to combine all those cases operationally
so that they are tried until one applies. The result of the operation
can be of many different types though (literals, additions, etc.), and
hence, we also need to exploit a form of polymorphism to express that
result type. An attempt follows, but it fails, as we will discuss
below.

-}

-- The homogenized type of all expressions

data AnyExp = forall x. (Exp x, Show x) => AnyExp x

instance Show AnyExp
 where
  show (AnyExp x) = show x


-- Apply a polymorphic function on expressions

applyToExp :: (forall x. (Exp x, Show x) => x -> y) -> AnyExp -> y
applyToExp f (AnyExp x) = f x


-- The conversion from trees to expressions

tree2exp :: Tree String -> AnyExp

tree2exp (Node "Lit" [i])   = AnyExp (Lit (fromTree i))

tree2exp (Node "Neg" [x])   = applyToExp (AnyExp . Neg)
                                         (tree2exp x)

tree2exp (Node "Add" [x,y]) = applyToExp (applyToExp (\x' -> AnyExp . Add x')
                                                     (tree2exp x))
                                         (tree2exp y)


{-

One problem with this attempt is that it is actually no longer open
(extensible). The given function takes a closed-world assumption on
the served forms of trees (corresponding to known forms of
expressions). This problem could be potentially solved by sort of
chaining up "blocks" like the one above with the help of maybes. The
bigger problem is the representation of the result. When we
existentially quantify, we must also mention all type-class
constraints that are needed perhaps. The type AnyExp mentions Exp as
the only constraint. This leaves us virtually with no operation to be
applied to the result of de-serialization. For instance, if we wanted
to print the de-serialized expression, we better add a printing
constraint to AnyExp--but how can we possibly anticipate all
operations to be applied to the results of de-serialization?

We call this the de-serialization problem:

"Can we describe functionality for de-serialization in a modular
fashion so that arbitrary functionality can be applied to the data,
just as if the data was never serialized in the first place?"

-}
