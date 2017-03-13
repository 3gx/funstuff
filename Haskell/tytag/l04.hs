-- Chapter 4 from lecture.pdf in  
--   http://okmij.org/ftp/tagless-final/course/index.html

{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ((^))

-- 4.2 Typed formatting
-----------------------

-- Printer/parsers (injection/projection pairs)
data PrinterParser a = PrinterParser (a -> String) (String -> Maybe (a,String))

class FormattingSpec repr where
    lit  :: String -> repr a a
    int  :: repr a (Int -> a)
    char :: repr a (Char -> a)
--    fpp  :: PrinterParser b -> repr a (b -> a)
    (^)  :: repr b c -> repr a b -> repr a c
infixl 5 ^

newtype FPr a b = FPr ((String -> a) -> a)

instance FormattingSpec FPr where
  lit str = FPr $ \k -> k str
  int     = FPr $ \k -> \x -> k (show x)
  char    = FPr $ \k -> \x -> k [x]
--    fpp (PrinterParser pr _) = FPr $ \k -> \x -> k (pr x)
  (FPr a) ^ (FPr b)  = FPr $ \k -> a (\sa -> b (\sb -> k (sa ++ sb)))


fmt :: (FormattingSpec repr, Show b, Read b) => b -> repr a (b -> a)
fmt x = fpp showread

sprintf :: FPr String b -> b
sprintf (FPr fmt) = fmt id


tp1 = sprintf $ lit "Hello world"
-- "Hello world"
ts1 = sscanf "Hello world" (lit "Hello world")  ()
-- Just ()

tp2 = sprintf (lit "Hello " ^ lit "world" ^ char) '!'
-- "Hello world!"
ts2 = sscanf "Hello world!" (lit "Hello " ^ lit "world" ^ char) id
-- Just '!'
