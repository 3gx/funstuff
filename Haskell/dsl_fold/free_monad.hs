-- output b -- prints a "b" to the console
-- bell     -- rings the computer bell
-- done     -- end of execution

{-# LANGUAGE UndecidableInstances #-}


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
subroutine = Fix (Output 'A' (Throw IncompleteException))
   :: FixE (Toy Char) IncompleteException

-- try {subroutine}
-- -- catch (IncompleteException) {
-- --     bell
-- --     done
-- -- }
program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))
-- :: FixE (Toy Char) e
  :: FixE (Toy Char) Int    -- Make e = Int otherwise print program complains about type e

program' = catch'  (\_ -> Fix (Bell (Fix Done))) subroutine
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


main = do
  print subroutine
  print program
  print program'
