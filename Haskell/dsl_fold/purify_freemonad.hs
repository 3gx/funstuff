{-# LANGUAGE DeriveFunctor, UndecidableInstances #-}

import System.Exit hiding (ExitSuccess)
--import Control.Monad.Free


{-
main = do x <- getLine
          putStrLn x
          exitSuccess
          putStrLn "Finished"
-}

data Free f r = Free (f (Free f r)) | Pure r deriving (Functor)

-- instance of Show for Free f
instance (Show (f (Free f r)), Show r) => Show (Free f r) where
  show (Free f) = "(Free " ++ show f ++ ")"
  show (Pure r) = "(Pure " ++ show r ++ ")"


-- instance of Applicative for Free f
instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> a = fmap f a
  Free f <*> a = Free $ fmap (<*> a) f

-- instance of Monad for Free f
instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)


data TeletypeF x = PutStrLn String x
                 | GetLine (String -> x)
                 | ExitSuccess 

instance (Show x) => Show (TeletypeF x) where
  show (PutStrLn str z) = "(PutStrLn String " ++ show str ++ " " ++ show z ++ ")"
  show (GetLine f) = "GetLine"
  show (ExitSuccess) = "ExitSuccess"

instance Functor TeletypeF where
  fmap f (PutStrLn str x) = PutStrLn str (f x)
  fmap f (GetLine      k) = GetLine (f . k)
  fmap f ExitSuccess      = ExitSuccess

type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

exitSuccess' :: Teletype r
exitSuccess' = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r) = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine      f)) = getLine      >>= run . f
run (Free ExitSuccess     ) = exitSuccess

runPure :: Teletype r -> [String] -> [String]
runPure (Pure r)              xs  = []
runPure (Free (PutStrLn y t)) xs  = y:runPure t xs
runPure (Free (GetLine k))    []  = []
runPure (Free (GetLine k)) (x:xs) = runPure (k x) xs
runPure (Free ExitSuccess)    xs  = []


echo :: Teletype ()
echo = do 
  str <- getLine'
  putStrLn' str
  exitSuccess'
  putStrLn' "Finished"

main = run echo
