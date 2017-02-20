{-# LANGUAGE TemplateHaskell #-}

module Th04 (module Th04) where

import Language.Haskell.TH

isPrime :: (Integral a) => a -> Bool
isPrime k | k <= 1 = False 
          | otherwise = not $ elem 0 (map (mod k) [2..k-1])

nextPrime :: (Integral a) => a -> a
nextPrime n | isPrime n = n
            | otherwise = nextPrime (n+1)

doPrime :: (Integral a) => a -> a -> [a]
doPrime n m  | curr > m = []
             | otherwise = curr:doPrime (curr+1) m
           where curr = nextPrime n

primeQ :: Int -> Int -> ExpQ
primeQ n m = [| doPrime n m |]
