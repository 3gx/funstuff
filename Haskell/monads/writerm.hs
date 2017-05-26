{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

newtype Writer w a = Writer { runWriter :: (a,w) }

instance (Monoid w) => Functor (Writer w) where
  fmap f (Writer (a,w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (Writer (f,w)) <*> (Writer (x,w')) = Writer (f x, w `mappend` w')

instance (Monoid w) => Monad (Writer w) where
 return a = Writer $ (a,mempty) 
 -- (>>=) :: Wrtier w a -> (a -> Writer w b) -> Writer w b
 (Writer (a,w)) >>= f = 
                        Writer $ let (b, w') = runWriter $ f a
                                 in  (b, w `mappend` w')

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  pass   :: m (a, w -> w) -> m a
  listen :: m a -> m (a, w)
  tell   :: w -> m ()

instance (Monoid w) => MonadWriter w (Writer w) where
  pass (Writer ((a,f),w)) = Writer $ (a, f w)
  listen (Writer (a,w)) = Writer ((a,w),w)
  tell s = Writer ((),s)

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a,b)
listens f m = do (a,w) <- listen m; return (a,f w)
 
censor :: (MonadWriter w m) => (w -> w) -> m a -> m a 
censor f m = pass $ do a <- m; return (a,f)

data Entry = Log { count :: Int, msg :: String } deriving (Eq, Show)

logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries [] x = return x
mergeEntries x [] = return x
mergeEntries [e1] [e2] = let (Log n msg) = e1
                             (Log n' msg') = e2
                         in if msg == msg' then
                              return [(Log (n+n') msg)]
                            else
                              do tell [e1]
                                 return [e2]
