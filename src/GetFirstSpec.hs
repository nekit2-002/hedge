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

module GetFirstSpec where

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

foreign import capi "csrc/struct.h getfst" getfst :: Ptr Foo -> IO CInt

class CategAlgebra ioObj => GetFirstSpec ioObj where
  fooarg :: (LogicAlgebra (ioObj Foo), Categorical Foo) =>
    ioObj Foo
  resint :: (LogicAlgebra (ioObj CInt), Categorical CInt) =>
    ioObj CInt

  getFstM :: Morphism ioObj
  getFM :: Morphism ioObj

  getfstSp :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt) => 
    (String, Prop (Hom ioObj))
  getfstSp = ("GetFirstSpec", sp)
    where
      fooH = hom @ioObj fooarg resint
      sp = eql fooH (UU $ getFstM @ioObj) (UU $ getFM @ioObj)

  getFirstSpec :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt, LogicAlgebra (ioObj ()), Categorical ()) =>
    ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  getFirstSpec = (getfstSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U fooarg, U resint]

instance GetFirstSpec NamedIOSet where
  fooarg = NamedIOSet "Struct Foo" fooGen fooCoGen
  resint = NamedIOSet "Int" (CInt <$> int32 constantBounded) vary
  getFstM = IOMorphism $ \(foo' :: IO Foo) -> do
    foo <- foo'
    fooptr <- new foo
    getfst fooptr
  getFM = IOMorphism $ \(foo' :: IO Foo) -> fs <$> foo'