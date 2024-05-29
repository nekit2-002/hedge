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

module GetSndSpec where

import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Foreign
import Hedgehog.Function.Internal
import Hedgehog.Internal.Gen ( Gen, int32 )
import WriteReadSpec
import Data.Functor.Contravariant ( (>$<) )
import Hedgehog.Internal.Range
import GetFieldSpec
import CategAlgebra

foreign import capi "csrc/struct.h getsnd" getsnd :: Ptr Foo -> IO CInt

class CategAlgebra ioObj => GetSndSpec ioObj where
  foostruct :: (LogicAlgebra (ioObj Foo), Categorical Foo) =>
    ioObj Foo
  intres :: (LogicAlgebra (ioObj CInt), Categorical CInt) =>
    ioObj CInt

  getSndM :: Morphism ioObj
  getSM :: Morphism ioObj

  getSndSp :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt) => 
    (String, Prop (Hom ioObj))
  getSndSp = ("GetSecondSpec", sp)
    where
      fooH = hom @ioObj foostruct intres
      sp = eql fooH (UU $ getSndM @ioObj) (UU $ getSM @ioObj)

  getSndSpec :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt, LogicAlgebra (ioObj ()), Categorical ()) =>
    ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  getSndSpec = (getSndSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U foostruct, U intres]

instance GetSndSpec NamedIOSet where
  foostruct = NamedIOSet "Struct Foo" fooGen fooCoGen
  intres = NamedIOSet "Int" (CInt <$> int32 constantBounded) vary
  getSndM = IOMorphism $ \(foo' :: IO Foo) -> do
    foo <- foo'
    fooptr <- new foo
    getsnd fooptr
  getSM = IOMorphism $ \(foo' :: IO Foo) -> s <$> foo'
