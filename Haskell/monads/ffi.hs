-- See:
-- https://wiki.haskell.org/IO_inside#Interfacing_with_C.2FC.2B.2B_and_foreign_libraries_.28under_development.29

main = do print "Hello from main"
          c_function

haskell_function = print "Hello from haskell_function"

foreign import ccall safe "prototypes.h"
  c_function :: IO ()

foreign export ccall
  haskell_function :: IO () 

