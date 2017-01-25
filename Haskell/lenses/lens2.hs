-- https://rafal.io/posts/haskell-lenses-notes.html --

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Lens.TH

data Person = P { _name :: String
                , _addr :: Address
                , _salary :: Int }

data Address = A { _road :: String
                 , _city :: String
                 , _postcode :: String }

$(makeLenses ''Person)
$(makeLenses ''Address)

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity a deriving (Show, Functor)

runIdentity :: Identity s -> s
runIdentity (Identity x) = x

over :: Lens' s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f) 

set :: Lens' s a -> a -> s -> s
set ln = (over ln) . const

newtype Const v ignore_type = Const v deriving (Show,Functor)
--instance Functor (Const v) where
--  fmap f (Const x) = Const x

view :: Lens' s a -> s -> Const a s
view ln = ln Const

--(.~) = set
--(%~) = view

setPostcode :: String -> Person -> Person
setPostcode pc p = set (addr . postcode) pc p
-- setPostcode pc p = addr.postcode .~ pc $ p
--                                   |___ (.~)  = set
--                                   |___ f $ x = f x

-- -= Applications =-

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


