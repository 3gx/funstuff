-- output b -- prints a "b" to the console
-- bell     -- rings the computer bell
-- done     -- end of execution

{-# LANGUAGE UndecidableInstances #-}

import GHC.IO
import Control.Monad


data Toy b next = Output b next
                | Bell next
                | Done 
            deriving (Show)

data FixE f e = Fix (f (FixE f e)) | Throw e 


instance (Show (f (FixE f e)), Show e)=> Show (FixE f e) where
  show (Fix f) = "(Fix " ++ show f ++ ")"
  show (Throw e) = "(Throw " ++ show e ++ ")"


catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e

catch' :: (Functor f) => (e1 -> FixE f e2) -> FixE f e1 -> FixE f e2
catch' f (Fix x) = Fix (fmap (catch' f) x)
catch' f (Throw e) = f e

instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell     next) = Bell     (f next)
  fmap f  Done           = Done

-- code
data IncompleteException = IncompleteException deriving (Show)

-- throw IncompleteException
subroutine'' = Fix (Output 'A' (Throw IncompleteException))
   :: FixE (Toy Char) IncompleteException

-- try {subroutine}
-- -- catch (IncompleteException) {
-- --     bell
-- --     done
-- -- }
program'' = subroutine'' `catch` (\_ -> Fix (Bell (Fix Done)))
-- :: FixE (Toy Char) e
  :: FixE (Toy Char) Int    -- Make e = Int otherwise print program complains about type e

program' = catch'  (\_ -> Fix (Bell (Fix Done))) subroutine''
  :: FixE (Toy Char) Int  

-- free monads, I

data Free f r = Free (f (Free f r)) | Pure r

-- instance of Show for Free f
instance (Show (f (Free f r)), Show r) => Show (Free f r) where
  show (Free f) = "(Free " ++ show f ++ ")"
  show (Pure r) = "(Pure " ++ show r ++ ")"

-- instance of Functor for Free f
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free (fmap (fmap f) x)

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

output :: a -> Free (Toy a) ()
--output x = Free (Output x (Pure ()))

bell :: Free (Toy a) ()
--bell = Free (Bell (Pure ()))

done :: Free (Toy a) r
--done = Free Done

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output x = liftF (Output x ())
bell = liftF (Bell  ())
done = liftF Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

-- program :: Free (Toy Char) r
program :: Free (Toy Char) Int -- set r = Int to work in main
program = do
  subroutine
  bell
  done

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) = "bell\n" ++ showProgram x
showProgram (Free Done) = "done\n"
showProgram (Pure r) = "return " ++ show r ++ "\n"

ringBell :: IO()
ringBell = print "Ding!"

interpret :: (Show b) => Free (Toy b) r -> IO()
interpret (Free (Output b x)) = print b  >> interpret x
interpret (Free (Bell     x)) = ringBell >> interpret x
interpret (Free (Done      )) = return()
interpret (Pure r) = throwIO (userError " Improper termination")

-- Concurrency

data Thread m r = Atomic (m (Thread m r)) | Return r 

-- instance of Show for Thread m r
instance (Show (m (Thread m r)), Show r) => Show (Thread m r) where
  show (Atomic f) = "(Atomic " ++ show f ++ ")"
  show (Return r) = "(Return " ++ show r ++ ")"

atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ liftM Return m

instance (Monad m) => Functor (Thread m) where
  fmap = liftM

instance (Monad m) => Applicative (Thread m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (Thread m) where
  return = Return
  (Atomic m) >>= f = Atomic (liftM (>>= f) m)
  (Return r) >>= f = f r
thread1 :: Thread IO()
thread1 = do
  atomic $ print 1
  atomic $ print 2

thread2 :: Thread IO()
thread2 = do
  str <- atomic $ getLine
  atomic $ putStrLn str

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

interleave :: (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
  next1 <- atomic m1
  next2 <- atomic m2
  interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

main = do
  print subroutine''
  print program''
  print program'
  print " ------- "
  putStr $ showProgram program
  print " ------- "
  pretty program
  runThread $ interleave thread1 thread2

  
