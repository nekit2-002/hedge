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

module FindSpec where

import Foreign.C.Types
import GHC.Generics
import Data.Int
import Data.Functor.Contravariant ( (>$<) )
import Data.Typeable
import Hedgehog.Function.Internal
import Hedgehog.Internal.Gen
import WriteReadSpec
import CategAlgebra
import Hedgehog.Internal.Range (constantBounded, constant)

foreign import capi "csrc/find.h transform" transform :: CInt -> IO CInt

newtype NCheck = NC {n :: Int32} deriving (Eq, Generic, Typeable)
instance Show NCheck where
  show (NC n) = show n

instance Arg NCheck

class CategAlgebra ioObj => FindSpec ioObj where
  numsC :: (LogicAlgebra (ioObj NCheck), Categorical NCheck) =>
    ioObj NCheck

  transformM :: Morphism ioObj
  transformId :: (LogicAlgebra (ioObj NCheck), Categorical NCheck) =>
    (String, Prop (Hom ioObj))
  transformId = ("TransformIdEq", sp)
    where
      homNumC = hom @ioObj numsC numsC
      sp = eql homNumC (UU $ idm @ioObj numsC) (UU $ transformM @ioObj)

  transrormIdSpec :: (LogicAlgebra (ioObj ()), LogicAlgebra (ioObj NCheck), Categorical (),
    Categorical NCheck) => ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  transrormIdSpec = (transformId @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U numsC]

-- ! -------------------------------- !--
-- ! Instances !--

instance FindSpec NamedIOSet where
  numsC = NamedIOSet "Int" numCGen numCCoGen
  transformM = IOMorphism $ \(nc :: IO NCheck) -> do
    (NC n) <- nc
    let nc' = CInt n 
    (CInt n') <- transform nc'
    pure $ NC n'

numCGen :: Gen NCheck
numCGen = NC <$> int32 constantBounded

numCCoGen :: CoGen NCheck
numCCoGen = (\(NC n) -> n) >$< vary