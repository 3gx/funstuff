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
 --  :: FixE (Toy Char) e
