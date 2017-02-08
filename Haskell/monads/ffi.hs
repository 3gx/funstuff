main = do print "Hello from main"
          c_function

haskell_function = print "Hello from haskell_function"

foreign import ccall safe "prototypes.h"
  c_function :: IO ()

foreign export ccall
  haskell_function :: IO () 

