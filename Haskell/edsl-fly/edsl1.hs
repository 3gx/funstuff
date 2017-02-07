{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

--import Prelude hiding ((<))

import LLVM.Core 
import LLVM.ExecutionEngine 


mkFcn :: CodeGenModule (Function (Float -> Float -> IO Float))
mkFcn = createFunction InternalLinkage $ \x y -> do
    x2 <- add x x
    tmp <- mul x2 y
    ret tmp

{-
mkFcn1 :: CodeGenModule (Function (Int -> Int -> IO Int))
mkFcn1 = createFunction InternalLinkage $ \x y -> do
    x2 <- add x x
    tmp <- mul x2 y
    ret tmp
-}

main = do
  initializeNativeTarget
  fcnIO <- simpleFunction mkFcn
  let fcn :: Float -> Float -> Float
      fcn = unsafePurify fcnIO
  print $ fcn 2 3.3 
 
{- 
  fcnIO1 <- simpleFunction mkFcn1
  let fcn1 :: Int -> Int -> Int
      fcn1 = unsafePurify fcnIO1
  print $ fcn1 2 3
-}
