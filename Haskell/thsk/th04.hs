{-# LANGUAGE TemplateHaskell #-}

-- https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html

import Th04
import Language.Haskell.TH

primes= $(primeQ 0 2390)

main = do
  runQ [d| x= 5|] >>= print
  runQ [t| Int |] >>= print
  runQ [p|(x,y)|] >>= print
  print $ length $ primes
