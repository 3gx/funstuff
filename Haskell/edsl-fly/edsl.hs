{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

--import Prelude hiding ((<))

--import LLVM.Core
-- import LLVM.General.ExecutionEngine


{- 
  data Date   -- type of dates
  data Asset  -- type of asserts
  data List a -- lists

  (+), (-), (*), (/) :: EDouble -> EDouble -> EDouble
  max, max           :: EDouble -> EDouble -> EDouble
  abs, log, expr     :: EDouble -> EDouble
  (<)                :: EDouble -> EDouble -> EBool
  observe            :: Asset -> Date -> EDouble
  cond               :: EBool -> a -> a -> a
  foldl1             :: (a->a->a) -> List a -> a
  map                :: (a->b) -> List a -> List b

  bestOf :: List Assert -> Date -> Date -> EDouble
  bestOf assets startDate endData =
    foldl1 max $ map (perf startData endData) assets

  perf :: Date -> Date -> Assert -> EDouble
  perf t1 t2 assert = observe assert t2 / obaserve asset t1 - 1

  cliquet :: (Asset, EDouble, EDouble, EData, List Data) -> EDouble
  cliquet (asset, floor, cap, initDate, dates) =
    max floor $  min cap val
      where cliquetPerf (prevDate, prevSum) currDate = 
               (currData, prevSum + currPerf)
                  where currPerf = perf prevDate currDate asset 
            (_,val) = foldl cliquetPerf (initDate, 0) dates 
-}


-- Shallow Embedding

-- Date type is simple days from some start
newtype Date = Date { unDate :: Integer }  deriving Show

-- Asset type is used to get the value of an asset at a specific date.
-- Make it a function
data Asset = Asset (Date -> Double)  

instance Show Asset where
  show a = "Asset"

{-
type EDouble = Double
type EBool = Bool
type List a = [a]

observe :: Asset -> Date -> EDouble
observe (Asset f) d = f d

cond :: EBool -> a -> a -> a
cond c t e = if c then t else e
-}
-- Deep embedding


data Expr
      -- Functions
      = Add Expr Expr
      | Sub Expr Expr
      | Mul Expr Expr
      | Div Expr Expr
      | Log Expr
      | Exp Expr
      | Less Expr Expr
      | Cond Expr Expr Expr
      | Observe Expr Expr
      -- Constants
      | EDouble Double
      | EBool Bool
      | EAsset Asset
      | EDate Date  
      -- deep embedding of lists
  --  | EFoldl1 (Expr -> Expr -> Expr) Expr
  --  | EMap (Expr -> Expr Expr
  --  | EList [Expr]
  deriving Show


-- Instead, a phatom type version will be exposed

data E a = E Expr         deriving Show
type EDouble = E Double
type EBool = E Bool

{-
 (+), ... :: E Double -> E Double -> E Double
 (<), ... :: E Double -> E Double -> E bool
 observe  :: E Asset  -> E Date -> E Double
 cond     :: E Bool   -> a -> a -> a
 -}

-- requires FlexibleInstances 
instance Num EDouble where
  (+) = binOp Add
  (-) = binOp Sub
  (*) = binOp Mul
  abs x = cond (x .<. 0) (-x) x
  fromInteger = E . EDouble . fromInteger
  signum x = cond (x .<. 0) (-1) (cond (0 .<. x) 1 0)

binOp :: (Expr -> Expr -> Expr) -> E a -> E b -> E c
binOp op (E x) (E y) = E (op x y)

unOp :: (Expr -> Expr) -> E a  -> E a
unOp op (E x) = E (op x) 

instance Fractional EDouble where
  (/) = binOp Div
  fromRational = E . EDouble . fromRational

-- generates warning because of missing impl for
--   pi, sin, cos, asin, acos, atan, sinh, cosh,
--   asinh, acosh & atanh
instance Floating EDouble where
  exp = unOp Exp
  log = unOp Log

{-
observe :: E Asset -> E Date -> EDouble
observe = binOp Observe
-}
observe :: Asset -> Date -> Double
observe (Asset f) d = f d


{-
infix 4 <
(<) :: E Double -> E Double -> E Bool
(<) = binOp Less
-}

infix 4 .<.
(.<.) :: E Double -> E Double -> E Bool
(.<.) = binOp Less

cond :: E Bool -> E a -> E a -> E a
cond (E c) (E t) (E e) = E (Cond c t e)


class Value a where
  lift :: a -> E a
  down :: E a -> a

instance Value Double where
  lift = E . EDouble
  down (E (EDouble x)) = x

instance Value Date where
  lift = E . EDate
  down (E (EDate x)) = x

instance Value Bool where
  lift = E . EBool
  down (E (EBool x)) = x

instance Value Asset where
  lift = E . EAsset
  down (E (EAsset x)) = x

-- Deep
--   type List a = E [a]
-- Shallow
--   type List a = [a]

-- list exist at "compile time"  (when haskell runs)
--   bestOf :: [E Asset] -> E Date -> E Date -> EDouble
-- list exist at "compile time"
--  bestOf :: E [Asset] -> E Date -> E Date -> EDouble

eval :: Expr -> Expr
eval (Add e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 + x2) 

eval (Sub e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 - x2) 

eval (Mul e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 * x2) 

eval (Div e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EDouble (x1 / x2) 

eval (Less e1 e2) = case (eval e1, eval e2) of
                    (EDouble x1, EDouble x2) -> EBool (x1 < x2) 
eval (Log e) = case eval e of EDouble x -> EDouble (log x)
eval (Exp e) = case eval e of EDouble x -> EDouble (exp x)

eval (Observe e1 e2) = case (eval e1 , eval e2) of
                          (EAsset (Asset f), EDate d) -> EDouble (f d)

eval e = e 

evalE :: Value a => E a -> a
evalE (E e) = down $ E $ eval e
expr = 1+2::EDouble

-- > evalE expr
-- 3.0

-- Test
d1,d2 :: Date
d1 = Date 100
d2 = Date 200

a1, a2 :: Asset
a1 = Asset $ const 5
a2 = Asset $ \(Date d) -> 0.1 * fromIntegral (d-10)
  
type List a = [a]

-- Neritic embeding (?!?)

bestOf :: List (E Asset) -> E Date -> E Date -> EDouble
bestOf assets startDate endDate =
   lift $ (foldl1 max $ map (\x -> perf (evalE startDate)  (evalE endDate) (evalE x)) assets)

perf :: Date -> Date -> Asset -> Double
perf t1 t2 asset = observe asset t2 / observe asset t1 - 1

t :: Double
t = evalE $ bestOf [lift a1, lift a2] (lift d1) (lift d2)

-- > t
-- 1.1111111111111112

-- LLVM [35m in video]

-- Use LLVM JIT
--    * Build instruction sequence
--    * Call JIT
--    * Returns a pointer to the code

-- LLVM follows C calling convention
-- There is GCH LLVM backend

-- \x y -> (x+x)*y

{-
mkFcn :: CodeGenModule (Function (Double -> Double -> IO Double))
mkFcn = createFunction InternalLinkage $ \x y -> do
    x2 <- add x x
    tmp <- mul x2 y
    ret tmp

main = IO ()
main = do
  initializeNativeTarget
  fcnIO <- simpleFunction mkFcn
  let fcn :: Double -> Double -> Double
      fcn = unsafePurify fcnIO
  print $ fcn 2 3   
-}
