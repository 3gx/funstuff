{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

getPassphrase :: IO (Maybe String)
getPassphrase = do 
  s <- getLine
  if isValid s then return $ Just s else return Nothing

isValid :: String -> Bool
isValid = ("pass"==)

askPassphrase :: IO ()
askPassphrase = do
  putStrLn "Insert your new passphrase:"
  maybe_value <- getPassphrase
  if isJust maybe_value
  then do putStrLn "Yup!"
  else putStrLn "Nope:("


{-
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap f (MaybeT mx) = MaybeT $ mx >>= return . fmap f -- \x -> return (fmap f x)
                                            
instance Monad m => Applicative (MaybeT m) where
--  pure = MaybeT . return . Just
  pure = MaybeT . return . return
--  pure x = MaybeT $ return (Just x)
--  pure x = MaybeT $ return (return x)
--  (MaybeT mf) <*> (MaybeT mx) = MaybeT $ mf >>= \f -> mx >>= return . (f <*>) -- \x -> return (f <*> x)
  (MaybeT mf) <*> (MaybeT mx) = MaybeT $ mf >>= \f -> mx >>= return . (f <*>) -- \x -> return (f <*> x)

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  
  (MaybeT mx) >>= f = MaybeT $ mx >>= \x -> 
                                case x of 
                                  Nothing -> return Nothing
                                  Just x  -> runMaybeT $ f x

instance Monad m => Alternative (MaybeT m) where
  -- empty :: MaybeT m a 
  empty = MaybeT $ return Nothing

  -- (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  (MaybeT ma) <|> (MaybeT mb) = MaybeT $ ma >>= \a -> 
                                          case a of
                                            Nothing -> mb
                                            _       -> ma

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)
                               
instance MonadTrans MaybeT where
  -- lift :: m a -> MaybeT m a
--  lift mx = MaybeT $ mx >>= \x -> return (Just x)
    lift = MaybeT . (liftM Just)
-}

getPassphrase' :: MaybeT IO String
getPassphrase' = do 
      s <- lift getLine
      guard (isValid s)
      return s 

askPassphrase' :: MaybeT IO ()
askPassphrase' = do
    lift $ putStrLn "Insert new passphrase:"
    value <- getPassphrase'
    lift $ putStrLn "Strogin stuff"


