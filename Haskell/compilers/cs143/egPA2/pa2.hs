-- Programming Assignment I --

import System.Environment

printString :: String -> IO()
printString s = do
  print s


main :: IO ()
main = do
  [f] <- getArgs
  source <- readFile f
  print source
