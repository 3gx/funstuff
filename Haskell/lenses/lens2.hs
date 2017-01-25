-- https://rafal.io/posts/haskell-lenses-notes.html --

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Lens.TH
--import qualified Data.Map as Map
import Data.Char -- toLower

data Person = P { _name :: String
                , _addr :: Address
                , _salary :: Int } deriving Show

data Address = A { _road :: String
                 , _city :: String
                 , _postcode :: String } deriving Show

$(makeLenses ''Person)
$(makeLenses ''Address)

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity a deriving (Show, Functor)

-- https://hackage.haskell.org/package/transformers-0.2.2.0/docs/src/Data-Functor-Identity.html
instance Applicative (Identity) where
  pure a = Identity a
  Identity f <*> Identity x = Identity (f x)
  


runIdentity :: Identity s -> s
runIdentity (Identity x) = x

over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f) 

set :: Lens' s a -> a -> s -> s
set ln = (over ln) . const

newtype Const v ignore_type = Const v deriving (Show,Functor)
--instance Functor (Const v) where
--  fmap f (Const x) = Const x

--instance Applicative (Const v) where
--  pure x = Const x
 -- Const f <*> Const v = Const (f `mappend` v)

view :: Lens' s a -> s -> Const a s
view ln = ln Const

--(.~) = set
--(%~) = view

setPostcode :: String -> Person -> Person
setPostcode pc p = set (addr . postcode) pc p
-- setPostcode pc p = addr.postcode .~ pc $ p
--                                   |___ (.~)  = set
--                                   |___ f $ x = f x

-- ~= Applications =~

data Temp = Temp { _fahrenheit :: Float } deriving Show
$(makeLenses ''Temp)
--fahrenheit :: Lens Temp Float

centigrade :: Lens' Temp Float
centigrade centi_fn (Temp faren) = 
      (\centi' -> Temp (cToF centi')) <$> (centi_fn (fToC faren))

cToF :: Float -> Float -- Centigrade to Fahrenheit
cToF c = c*9 / 5 + 32

fToC :: Float -> Float -- Fahrenheit to Centigrade
fToC f = (f-32)*5 / 9

temp100 :: Temp
temp100 = Temp 100
-- > view centigrade temp100
-- Const 37.77778
-- > set centigrade 100 temp100
-- T {_fahrenheit = 212.0}
--

data Time = Time { _hours :: Int, _mins :: Int } deriving Show
$(makeLenses ''Time)

now = Time { _hours = 3, _mins = 58 }

mins' :: Lens' Time Int
mins' min_fn (Time h m) = wrap <$> (min_fn m)
  where 
    wrap :: Int -> Time
    wrap m' | m' >= 60 = Time (h+(m' `div` 60)) (m' `mod` 60)
            | m' <  0  = Time (h-1) (m'+60)
            | otherwise = Time h m'

-- > over mins' (+4) now
-- Time {_hours = 4, _mins = 2}


-- ~= Non-record structures =~

{-   Control.Lens.At
 
  at :: Ord k => k -> Lens' (Map.Map k v) (Maybe v)
  at k mb_fn m = wrap <$> (mb_fn mv)
    where
      mv = Map.lookup k m
     
      wrap :: Maybe v -> Map.Map k v
      wrap (Just v') = Map.insert k v' m
      wrap Nothing   = case mv of
                        Nothing -> a
                        Just _ ->  Map.delete k m
-}

-- ~= Bit fields =~

{- Data.Bits.Lens
 
  bitAt :: Int -> Lens' Int Bool

  > view (bitAt 1) 3
  True
  > view (bitAt 1) 2 
  True
  > view (bitAt 1) 5
  False

  bitAt :: Bits b => Int -> Lens' b Bool
-}

-- ~= Web-scraper =~  

{-  package <hexpat-lens>
   p ^ .. _HTML' . to allNodes
                 . traverse . named "a"
                 . traverse . ix "href"
                 . filtered isLocal
                 . to trimSpaces
-}

--------------------------------
-- ~= Edwards' second insight =~
--------------------------------

-- Multi-focus lens

type Traversal' s a = 
     forall f. Applicative f => (a -> f a) -> (s -> f s)

-- 's' -> type of the container
-- 'a' -> type of the (multiple) foci

-- Applicative

{-
  class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

  a bit like Monad but weaker | class Monad m where
                              |   return :: a -> m a
                              |   (>>=)  :: m a -> (a -> m b) -> m b

  every Monad is Applicative
     | pure = return
     | mf <*> mx = do { f <- mf; x <- mx; return (f x) }

  but not vice verse
-}


-- road :: Lens' Address String  -- defined by $(makeLenses ''Address)
-- road elt_fn (A r c p) = (\r' -> A r' c p) <$> (elt_fn r)
--                                   |__ box with  | hole in it
--                                                 |_____ thing to put in the hole

-- addr_strs :: Traversal' Address String
-- addr_strs elt_fn (A r c p) = 
--   ...(\r' c' -> A r' c' p)...(elt_fn r)...(elt_fn c)
--                     |___ box with | two holes :: | Stirng -> String -> Address
--                                   |______________|___ :: f String    
--                                                  |___ :: f String

addr_strs :: Traversal' Address String
--addr_strs elt_fn (A r c p) = 
--   pure (\r' c' -> A r' c' p) <*> (elt_fn r) <*> (elt_fn c)

addr_strs elt_fn (A r c p) = 
   (\r' c' -> A r' c' p) <$> (elt_fn r) 
                         <*> (elt_fn c)


-- ~= Using Traversals =~

over' :: Traversal' s a -> (a -> a) -> s -> s
over' ln f = runIdentity . ln (Identity . f)

-- | class Functor f => Applicative f where
-- |    pure  :: a -> f a
-- |    (<*>) :: f (a -> b) -> f a -> f b

getConst :: Const v a -> v
getConst (Const x) = x

instance Monoid a => Applicative (Const a) where
  pure x = Const mempty
  (Const vf) <*> (Const va) = Const (vf `mappend` va)

-- | class Monoid m where
-- |   mempty  :: a
--     mappend :: a -> a -> a

--  definedin GHC.Base
-- | instance Monoid [a] where
-- | mempty = []
-- | mappend = (++)


--view' :: Traversal' s a -> (s -> a)
view' ln s = getConst (ln Const s)

fredA = A "72 Humberstone Rd" "Cambridge" "CB4 1JD"

-- > over' addr_strs (map toLower) fredA
-- A {_road = "72 humberstone rd", _city = "cambridge", _postcode = "CB4 1JD"}
-- > view' addr_strs fredA
-- "72 Humberstone RdCambridge"

-- ~= Non-uniform traversals =~

{-
    The foci of a traversal can be highly selective
      * Every alternate element of list
      * All the even elements of a tree
      * The 'name' fields of all records in a table whose
            'salary' fields is > $20,000
-}

-- ~= Composing traversals =`

{-   
     ln1 :: Lens'      s1 s2
     tr1 :: Traversal' s1 s2
     ln2 :: Lens'      s2 a
     tr2 :: Traversal' s2 a

     ln1 . ln2 :: Lens'      s1 a
     tr1 . tr2 :: Traversal' s1 a

     tr1 . ln2 :: Traversal' s1 a
     ln1 . tr2 :: Lens'      s1 a
-} 

-- =======> lens-3.9.1 
{-  It all rests on Haskell's abstraction facilities:
      * Type classes
      * Higher rank types
      * Higher kinded type variables

  QA: Records in Haskell, same field name in multiple records
  type class Has f, which check if record has a filed f
-}



