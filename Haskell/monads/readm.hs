{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe (maybe)
import Control.Monad (liftM)

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
  fmap f (Reader r) = Reader $ \e -> f (r e)
instance Applicative (Reader e) where
  pure a = Reader $ \_ -> a
  (Reader rf) <*> (Reader rx) = Reader $ \e -> (rf e) (rx e)

instance Monad (Reader e) where
  -- return :: a -> Reader e a
  return a = Reader $ \_ -> a

  -- (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e

class MonadReader e m | m -> e where
  ask   :: m e
  local ::  (e -> e) -> m a -> m a

instance MonadReader e (Reader e) where
  ask = Reader id
  local f (Reader r) = Reader $ \e -> r (f e)
 
asks :: (Monad m, MonadReader e m) => (e -> a) -> m a 
asks sel = ask >>= return . sel

--                Text   |   Variable |  Quote     |  Include                |  Compound
data Template = T String | V Template | Q Template | I Template [Definition] | C [Template]   deriving Show
data Definition = D Template Template   deriving Show

data Environment = Env {templates :: [(String,Template)],
                        variables :: [(String,String)]}   deriving Show

lookupVar :: String -> Environment -> Maybe String
lookupVar name env = lookup name (variables env)

lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = lookup name (templates env)

addDefs :: [(String, String)] -> Environment -> Environment
addDefs defs env = env { variables = defs ++ (variables env) }

resolveDef :: Definition -> Reader Environment (String,String) 
resolveDef (D t d) = do name  <- resolve t
                        value <- resolve d 
                        return (name, value)

-- resolve template into a stirng
resolve :: Template -> Reader Environment String
resolve (T s) = return s
resolve (V t) = do varName <- resolve t
                   varValue <- asks (lookupVar varName)
                   return $ maybe "" id varValue          
resolve (Q t) = do tmplName <- resolve t
                   body     <- asks (lookupTemplate tmplName) 
                   return $ maybe "" show body
resolve (I t ds) = do tmplName <- resolve t
                      body     <- asks (lookupTemplate tmplName)
                      case body of  
                         Just t' -> do defs <- mapM resolveDef ds
                                       local (addDefs defs) (resolve t')
                         Nothing -> return""
resolve (C ts) = (liftM concat) (mapM resolve ts)

type Envr = [(String, Int)] 

lookp :: String -> Envr -> Maybe Int
lookp str env = lookup str env

envr :: Envr
envr = [("abc",1), ("def",2), ("hij",3)]


