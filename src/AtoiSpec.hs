{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE NoStarIsType #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeFamilyDependencies #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE ConstrainedClassMethods #-}
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE CApiFFI #-}

module AtoiSpec where

import CategAlgebra
import WriteReadSpec ()
import Foreign.C.Types
import Foreign.C.String (CString, castCCharToChar)
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Hedgehog.Function.Internal (Arg)

{- |
  In C type signature is:
  
  int atoi(const char* str);
  
  Documemtation says:

  * str may contain +/- sign
  * apart from +/- str may contain only digits
-}
foreign import capi "stdlib.h atoi" atoi :: CString -> IO CInt

newtype NumS = NS {ns :: [CChar]} deriving (Typeable, Eq, Generic)
instance Show NumS where
  show (NS ns) = map castCCharToChar ns
instance Arg NumS

class CategAlgebra ioObj => AtoiSpec ioObj where
  nums :: (LogicAlgebra (ioObj NumS), Categorical NumS) =>
    ioObj NumS

  readNM :: Morphism ioObj
  atoiM :: Morphism ioObj

  atoiReadEq :: (LogicAlgebra (ioObj NumS), Categorical NumS) =>
    (String, Prop (Hom ioObj))
  atoiReadEq = ("AtoiReadEq", sp)
    where
      numHom = hom @ioObj nums nums
      sp = eql numHom (UU $ atoiM @ioObj) (UU $ readNM @ioObj)

  atoiReadSpec :: (LogicAlgebra (ioObj ()), LogicAlgebra (ioObj NumS), Categorical (),
    Categorical NumS) => ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  atoiReadSpec = (atoiReadEq @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U nums]



