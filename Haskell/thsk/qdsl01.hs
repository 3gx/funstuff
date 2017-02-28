import QDSL01

main = do
  let pow1 = $$(power 4)
  print $ pow1 3
  print $ (3*3*3*3 :: Float)
  return ()
