import QDSL01

import Language.Haskell.TH.Lib
import Language.Haskell.TH

main = do
  let pow1 = $$(power 4)
  print $ pow1 3
  print $ (3*3*3*3 :: Float)
  expr <- runQ $ power 4
  putStrLn.pprint.unType $ expr
  
  return ()
