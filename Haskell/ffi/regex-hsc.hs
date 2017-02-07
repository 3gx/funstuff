{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Regex where
  
  import Foreign
  import Foreign.C.Types

#include <pcre.h>
