{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Trans.Free -- from the `free` package 
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Writer

import Control.Monad
import Data.Sequence
import GHC.IO

data ThreadF next = Fork next next
                  | Yield next
                  | Done 
                  deriving (Functor)

type Thread = FreeT ThreadF

yield :: (Monad m) => Thread m ()
yield = liftF (Yield ())

done :: (Monad m) => Thread m r
done = liftF Done

cFork :: (Monad m) => Thread m Bool
cFork = liftF (Fork False True)

fork :: (Monad m) => Thread m a -> Thread m ()
fork thread = do
  child <- cFork
  when child $ do
    thread
    done

roundRobin :: (Monad m) => Thread m a -> m ()
roundRobin t = go (singleton t) -- Begin with a single thread
  where
    go ts' = case (viewl ts') of
      -- The queue is empty: we're done!
      EmptyL -> return ()

      -- The quie is non-empty: Process first thread
      t :< ts' -> do
        x <- runFreeT t -- Run this thread's effects
        case x of
          -- New threads go to the back of the queue
          Free (Fork t1 t2) -> go (t1 <| (ts' |> t2))

          -- Yielding threads go to the back of the queue
          Free (Yield t') -> go (ts' |> t')

          -- Thread done: Remove the thread from the queue  
          Free Done -> go ts'
          Pure _    -> go ts'

mainThread :: Thread IO ()
mainThread = do
  lift $ putStrLn "Forking thread #1"
  fork thread1
  lift $ putStrLn "Forking thread #2"
  fork thread2

thread1 :: Thread IO ()
thread1 = forM_ [1..10] $ \i -> do
    lift $ print i
    yield

thread2 :: Thread IO ()
thread2 = replicateM_ 3 $ do
    lift $ putStrLn "Hello"
    yield

logger :: Thread (Writer [String]) ()
logger  = do
  fork helper
  lift $ tell ["Abort"]
  yield
  lift $ tell ["Fail"]

helper :: Thread (Writer [String]) ()
helper = do
  lift $ tell ["Rety"]
  yield
  lift $ tell ["!"]

