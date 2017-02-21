-- https://github.com/leonidas/codeblog/blob/master/2013/2013-01-14-observable-sharing.md

{-# LANGUAGE TypeFamilies #-}

import Data.Unique (newUnique, Unique) 
import Data.Reify  hiding (Unique)
import Data.Traversable (traverse)
import Data.Maybe
import Data.Function (on)

data Person      = Person  String [Person] deriving Show
data Person' ref = Person' String [ref] deriving Show

instance MuRef Person where
    type DeRef Person = Person'

    mapDeRef f (Person name friends) =
        Person' name <$> traverse f friends 

data UniquePerson = UniquePerson
    { name     :: String
    , friends  :: [UniquePerson]
    , identity :: Unique
    } deriving Show

instance Show Unique where
  show _ = "Unique"

bob   = Person "Bob"   [fred, alice]
fred  = Person "Fred"  [bob]
alice = Person "Alice" [fred]


makeUnique :: Person -> IO UniquePerson
makeUnique p = do
    Graph nodes rootId <- reifyGraph p

    let makeIdentity (uid,_) = do
            uniq <- newUnique
            return (uid, uniq)

    identities <- mapM makeIdentity nodes

    let deref uid = UniquePerson name' uniqueFriends identity'
          where
            uniqueFriends = map deref friends'
            identity'     = fromJust $ lookup uid identities
            Person' name' friends' = fromJust $ lookup uid nodes

{-
  deref uid = UniquePerson name uniquefriends identity

    Person' friends
  
 -}

  --  id1 <- newUnique
 --   id2 <- newUnique
--    return $ UniquePerson "bob" [id1] id2

    return $ deref rootId

is :: UniquePerson -> UniquePerson -> Bool
is = (==) `on` identity


main = do
    let bob   = Person "Bob"   [fred, alice]
        fred  = Person "Fred"  [bob]
        alice = Person "Alice" [fred]

    bob' <- makeUnique bob
    let [fred', alice'] = friends bob'

    -- Test that Bob and Alice know the exact same Fred
    print $ fred' `is` (friends alice' !! 0)
    print $ fred' `is` (friends bob' !! 0)
    print $ fred' `is` (friends bob' !! 1)
    print $ alice' `is` (friends bob' !! 1)

