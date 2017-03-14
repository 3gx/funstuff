{-# LANGUAGE TypeFamilies #-}

class C a where
  type F a :: *
  inj :: a -> F a
  prj :: F a -> a

-- bar :: (C a) => F a -> F a
-- bar x = inj (prj x)

