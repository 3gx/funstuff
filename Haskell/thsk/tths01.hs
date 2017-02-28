-- type templated haskell
-- http://gmainland.blogspot.com/2013/05/type-safe-runtime-code-generation-with.html


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

{-
power :: Int -> (TExp (Int -> Int))
power n = [|| \x -> $$(go n [|| x ||]) ||]
    where 
       go :: Int -> TExp Int -> TExp Int
       go 0 x = [|| 1 ||]
       g0 n x = [|| $$x * $$(go (n-1) x) ||]      
-}

newtype Meta a = Meta { unMeta :: ExpQ }

e :: Meta Int
e = Meta [| 'c' |]

compose :: Meta (a->b) -> Meta (b->c) -> Meta (a->c)
compose (Meta l) (Meta r) = Meta [| $r . $l |]

mkPow :: Num a => Int -> Meta (a -> a)
mkPow 0 = Meta [| const 1 |]
mkPow n = Meta [| \x -> x * $(unMeta $ mkPow (n-1)) x |]


compose1 :: TExpQ (b -> c) -> TExpQ (a -> b) -> TExpQ (a -> c)
compose1 f g = [|| $$f . $$g ||]

mkPow1 :: Num a => Int -> TExpQ (a -> a)
mkPow1 0 = [|| const 1 ||]
mkPow1 n = [|| \x -> x + $$(mkPow1 (n-1)) x ||]

main = do
  e <- runQ $ unMeta $ mkPow 3
  print $ show e
  (runQ $ unMeta $ mkPow 3) >>= putStrLn.pprint
  exp <- runQ $ mkPow1 3
  print $ unType exp
  putStrLn.pprint.unType $ exp


