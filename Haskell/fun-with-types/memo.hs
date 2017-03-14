{-# LANGUAGE TypeFamilies #-}

-- 3.1 Type-directed memoization
--------------------------------

class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

g :: Bool -> Integer
g = fromTable (toTable f)

factorial n | n < 1 = 1 | otherwise = n * factorial (n-1)
fibonacci n | n < 2 = 1 | otherwise = fibonacci(n-1) + fibonacci(n-2)
f True = factorial 30000
f False = fibonacci 30

--------------------

instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum t _) (Left  v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v

f1 :: Either Bool Bool -> Integer
f1 v = case v of 
        Left x -> if x then factorial 30000 else factorial 30
        Right x -> if x then fibonacci 30 else fibonacci 10

g1 = fromTable . toTable $ f1

--------------------

instance (Memo a, Memo b) => Memo (a,b) where 
  newtype Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y


f2 :: (Bool,Bool) -> Integer
f2 (True,True) = factorial 30000
f2 (True,False) = factorial 30
f2 (False,True) = fibonacci 30
f2 (False,False) = fibonacci 10

g2 = fromTable . toTable $ f2


-- 3.2 Memoization for recrusive types
--------------------------------------

instance (Memo a) => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w))
  toTable f = TList (f [])
                    (toTable (\x -> toTable (\xs -> f (x:xs))))
  fromTable (TList t _) [] = t
  fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

f3 :: [Bool] -> Integer
f3 [] = -1
f3 [True,True,False]  = fibonacci 20
f3 [True,False,True]  = fibonacci 30
f3 [True,False,False]   = fibonacci 35
f3 [False,True,True]   = fibonacci 13
f3 [False,True,False]  = fibonacci 23
f3 [False,False,True]  = fibonacci 33
f3 [False,False,False]   = fibonacci 33

g3 = fromTable . toTable $ f3

-- 3.3 Generic finite maps
--------------------------


class Key k where
  data Map k :: * -> *
  empty :: Map k v
  lookp :: k -> Map k v -> Maybe v

instance Key Bool where
  data Map Bool elt = MB (Maybe elt) (Maybe elt)
  empty = MB Nothing Nothing
  lookp False (MB mf _) = mf
  lookp True  (MB _ mt) = mt

-- 3.4 Session types and their duality
--------------------------------------

data Stop = Done
newtype In a b = In (a -> IO b)
data   Out a b = Out a   (IO b)

add_server :: In Int (In Int (Out Int Stop))
add_server = In $ \x -> return $ In $ \y -> do putStrLn "Thinking"
                                               return $ Out (x+y) (return Done)

class Session a where
  type Dual a
  run :: a -> Dual a -> IO ()

instance (Session b) => Session (In a b) where
  type Dual (In a b) = Out a (Dual b)
  run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance (Session b) => Session (Out a b) where
  type Dual (Out a b) = In a (Dual b)
  run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

instance Session Stop where
  type Dual Stop = Stop
  run Done Done = return ()

add_client :: Out Int (Out Int (In Int Stop))
add_client = Out 3 $ return $ Out 4 $ do putStrLn "Waiting"
                                         return $ In $ \ z-> print z >> return Done

test1 = run add_server add_client
test2 = run add_client add_server

neg_server :: In Int (Out Int Stop)
neg_server = In $ \x -> do putStrLn "Thinking"
                           return $ Out (-x) (return Done)

instance (Session a, Session b) => Session (Either a b) where
  type Dual (Either a b) = (Dual a, Dual b)
  run (Left  y) (x,_) = run y x
  run (Right y) (_,x) = run y x

instance (Session a, Session b) => Session (a, b) where
  type Dual (a,b) = Either (Dual a) (Dual b)
  run (x,_) (Left  y) = run x y
  run (_,x) (Right y) = run x y

neg_client :: Out Int (In Int Stop)
neg_client = Out 5 . (putStrLn "Waiting" >>) . return
           $ In $ \z -> print z >> return Done

server :: (In Int (Out Int Stop),
           In Int (In Int (Out Int Stop)))
server = (neg_server, add_server)

client :: Either (Out Int (In Int Stop))
                 (Out Int (Out Int (In Int Stop)))
client = Right add_client

test3 = run server client >> run client server
