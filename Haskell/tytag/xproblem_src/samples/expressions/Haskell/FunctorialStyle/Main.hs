import Functor
import Base
import Extension


-- Test harness

term1 = In $ Literal 1
term2 = In $ RightF $ Plus (In $ LeftF $ Literal 1)
                           (In $ RightF $ Plus (In $ LeftF $ Literal 2) (In $ LeftF $ Literal 3))

test1 = evalLang  (term1 :: Lang)
test2 = evalLang2 (term2 :: Lang2)

main = do
          print test1
          print test2
