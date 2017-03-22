
newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

instance (Monad m) => Functor (StateT s m) where
  -- fmap :: (a->b) -> State s m a -> State s m b
  fmap f (StateT ms) = StateT $ \s -> do
                                 (a,s') <- ms s
                                 return (f a, s')
                                 
                                      
  -- ms :: s -> m (a,s)

instance (Monad m) => Applicative (StateT s m) where
  pure = return
  (StateT mf) <*> (StateT mx) = StateT $ 
    \s -> do
        (f,s') <- mf s
        (x,s'') <- mx s'
        return (f x, s'')
        
                                      

instance (Monad m) => Monad (StateT s m) where
  -- return  :: a -> StateT s m a
  return x = StateT $ \s -> return (x,s)

  -- (>>=) :: StateT s m a -> (a -> State s m b) -> State s m b
  (StateT mx) >>= f = StateT $ 
        \s -> do
            (x,s') <- mx s
            (runStateT $ f x) s'
