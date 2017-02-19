{-# LANGUAGE TemplateHaskell #-}

module Th02 (module Th02)
             where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

tupleReplicate :: Int -> Q Exp
tupleReplicate n = do 
    id <- newName "x"
    return $ LamE [VarP id] (TupE $ replicate n $ VarE id)

tupleReplicate' :: Int -> Q Exp
tupleReplicate' n = do 
    id <- newName "x"
    pat <- [p| x |]
--    return $ LamE [VarP id] (TupE $ replicate n $ VarE id)
    return $ LamE [pat] (TupE $ replicate n $ VarE id)

summ n = summ' n [| 0 |]
summ' 0 code = code
summ' n code = [| \x -> $(summ' (n-1) [|$code+x|]) |]

-- printf

-- Desribe a format string
data Format = D 
            | S
            | L String

-- Parse a format string
parse :: String -> String -> [Format]
parse ('%':'s':xs) rest = L rest : S : parse xs ""
parse ('%':'d':xs) rest = L rest : D : parse xs ""
parse ""           rest = [L rest]
parse (x:xs)       rest = parse xs (rest ++ [x])

-- Generaet Hasjell code from a parsed reproresentation
-- of the string. Thei code will be spliced into
-- the module which calls "printf", at compile time
gen :: [Format] -> ExpQ -> ExpQ
gen [] code = code
gen (D:xs)   code = [| \x-> $(gen xs [| $code ++ show x |] ) |]
gen (S:xs)   code = [| \x-> $(gen xs [| $code ++ x |] ) |]
gen (L s:xs) code = gen xs [| $code ++ s |]

-- Here we generate the haskell code for splice
-- from an input string
printf :: String -> ExpQ
printf s = gen (parse s "") [| "" |]

-- deriveShow

data T1 = T1
data T2 a = T2 a

{-
deriveShow t = do
  -- Get list of constructors of type t
  TyConI (DataD _ _ _ _ constructors _) <- reify t

  -- Make `show` clause for one constructor:
  --   show (A x1 x2) = "A" ++ show x1 ++ " " ++ show x2
  let showClause (NormalC name fileds) = do
			
			-- Name of constructor, i.e. "A". Will be come string literal in generated 
			-- code 
			let constructorName = nameBase name

			-- Get variables for left and right side of function definition
			(pats,vars) <- genPE (length fields)

			-- Recursively build (" " ++ show x1 ++ ... ++ "") expression from [x1...]
			-- variable list
			let f []       = [| "" |] 
			let f (v:vars) = [| " " ++ show $v ++ $(f vars) |] 

			-- Generate function clause for one constructor
			clause [conP name pats]           -- (A x1 x2)
						 (normal B [| iconstructorName ++ $(f vars) |]) [] -- "A" ++ show x1 ++ " " show x2


  -- Make body for function `show`:
  --    show (A x1 x2) = "A " ++ show x1 ++ " " ++ show x2
  --    show (B x1)    = "B " ++ show x1
  --    show C         = "C"
  showbody <- mapM showClause constructors

  -- Generate template instance declaration and then replace
  --   type name (T1) and function body (\x -> "text") with our data
  d <- [d| instance Show T1 where 
                show x = "text"
       |] 
  let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD [] (AppT showt (ConT t  )) [FunD showf showbody] ]
 
-- Generate n unique variables and return them in form of patterns and
-- expressions
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
     
-}

deriveShow t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ _ constructors _)  <-  reify t
 
  -- Make `show` clause for one constructor:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  let showClause (NormalC name fields) = do
        -- Name of constructor, i.e. "A". Will become string literal in generated code
        let constructorName = nameBase name
        -- Get variables for left and right side of function definition
        (pats,vars) <- genPE (length fields)
        -- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
        let f []       = [| "" |]
            f (v:vars) = [| " " ++ show $v ++ $(f vars) |]
        -- Generate function clause for one constructor
        clause [conP name pats]                                 -- (A x1 x2)
               (normalB [| constructorName ++ $(f vars) |]) []  -- "A "++show x1++" "++show x2
 
  -- Make body for function `show`:
  --   show (A x1 x2) = "A "++show x1++" "++show x2
  --   show (B x1)    = "B "++show x1
  --   show C         = "C"
  showbody <- mapM showClause constructors
 
  -- Generate template instance declaration and then replace
  --   type name (T1) and function body (\x -> "text") with our data
  d <- [d| instance Show T1 where
             show x = "text"
       |]
  let    [InstanceD Nothing [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD Nothing [] (AppT showt (ConT t  )) [FunD showf showbody]]
 
 
-- Generate n unique variables and return them in form of patterns and expressions
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)  

