import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity (Identity)

type Wr a = WriterT [String] Identity a
-- 

half :: Int -> Writer String Int
half x = do
        tell ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)

half1 :: Int -> Wr Int
half1 x = do
        tell (["I just halved " ++ (show x) ++ "!"])
        return (x `div` 2)

data EvalState = EvalState { depth :: Int } deriving Show

-- WriterT w m a = WriterT { runWriterT :: m (a,w) }

-- w = [String]
-- m = State EvalState
-- a = a
-- WrT a = WriterT { runWriterT :: State EvalState (a,[String]) }
--
-- State EvalState (a,[String]) = 
--       State $ { runState :: EvalState -> ((a,[String]),EvalState }

type WrT a = WriterT [String] (State EvalState) a

half2 :: Int -> WrT Int
half2  x = do
  -- enter inner monad (which is State Monad)
  --    obtain current depth
  --    increment depth
  d <- do  s <- get                         -- get state
           put $ s { depth = depth s + 1 }  -- increase depth by 1
           return $ depth s                 -- return depth

  -- base monad record the message
  tell ([show d ++ ":" ++ "I just halved " ++ (show x) ++ "!"]) 

  -- finall, we need to return the result of actions

  -- Path 1:
  -- -------
  -- wrap result into inner monad (this is state monad)
  let  st =  return (x `div`2, mempty)

  -- return inner monad
  WriterT  st

  -- or equivalently
  st1 <-  return (x `div`2, mempty)
  WriterT . return $ st1

  -- or simple
  WriterT . return $ (x `div` 2, mempty) 
 
  -- Path 2:
  -- -------
  -- alterantively, use this short-hand notation 
  return $ x `div` 2 

  -- Path 3:
  -- -------
  -- or something like this
  res <- WriterT st   
  return res
   

  


g x = ( evalState $ runWriterT $ (half2 >=> half2) x) $ (EvalState 0)
f = g 39
