{-# LANGUAGE DeriveFunctor #-}

data ThreadF next = Fork next next
                  | Yield next
                  | Done 
                  deriving (Functor)
