{-# LANGUAGE DeriveFunctor #-}

newtype ClientId = ClientId Integer deriving Show
data Client = Client { clientName :: String } deriving Show

data AdminOp r = GetClient  ClientId         (Client   -> r)
               | SaveClient ClientId  Client r
               | NewCLient  Client           (ClientId -> r)
          deriving Functor
