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

module PartialSumSpec where

import Foreign.Marshal.Array
import CategAlgebra
import Data.Int

import Foreign.C.Types 
import Foreign.Ptr ( Ptr )
import Foreign
import Hedgehog.Function.Internal
import GHC.Generics
import Data.Data (Typeable)
import Data.Functor.Contravariant
import qualified Hedgehog.Internal.Range as Range
import Hedgehog.Internal.Gen
import WriteReadSpec

foreign import capi "csrc/find.h sumFromChild" sumFromChild :: CInt -> Ptr CInt -> IO CLong
foreign import capi "csrc/find.h getSum" getSum :: CInt -> Ptr CInt -> IO CLong

newtype IntArr = Arr {array :: [Int32]} deriving (Eq, Typeable, Generic)
instance Show IntArr where
  show (Arr arr) = show arr
instance Arg IntArr

class CategAlgebra ioObj => PartialSumSpec ioObj where
  arr :: (LogicAlgebra (ioObj IntArr), Categorical IntArr) =>
    ioObj IntArr

  lres :: (LogicAlgebra (ioObj CLong), Categorical CLong) =>
    ioObj CLong

  getChildSumM :: Morphism ioObj
  getSumM :: Morphism ioObj

  partialEqSp :: (LogicAlgebra (ioObj IntArr), Categorical IntArr,
    LogicAlgebra (ioObj CLong), Categorical CLong) =>
    (String, Prop (Hom ioObj))
  partialEqSp = ("PartialSumChildSpec", sp)
    where
      rhom = hom @ioObj arr lres
      sp = eql rhom (UU $ getChildSumM @ioObj) (UU $ getSumM @ioObj)

  partialSumSpec :: (LogicAlgebra (ioObj ()), Categorical (),
    LogicAlgebra (ioObj IntArr), Categorical IntArr, LogicAlgebra (ioObj CLong),
    Categorical CLong) =>
    ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  partialSumSpec = (partialEqSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U arr, U lres]

instance PartialSumSpec NamedIOSet where
  arr = NamedIOSet "IntArr" g ((\(Arr arr) -> Prelude.map abs arr) >$< vary)
    where
      g = Arr <$> list (Range.constant 1 10) (int32 (Range.constant 0 maxBound))
  lres = NamedIOSet "Long" g ((\(CLong n) -> abs n) >$< vary)
    where
      g = CLong <$> int64 (Range.constant 0 maxBound)
  getChildSumM = IOMorphism $ \(arr' :: IO IntArr) -> do
    (Arr arr'') <- arr'
    let l = CInt . fromIntegral $ length arr''
    arr <- newArray (Prelude.map CInt arr'')
    sumFromChild l arr
  getSumM = IOMorphism $ \(arr' :: IO IntArr) -> do
    (Arr arr'') <- arr'
    let l = CInt . fromIntegral $ length arr''
    arr <- newArray (Prelude.map CInt arr'')
    getSum l arr
