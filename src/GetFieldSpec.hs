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

module GetFieldSpec where

import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Hedgehog.Function.Internal
import Hedgehog.Internal.Gen ( Gen, int32 )
import WriteReadSpec
import Data.Functor.Contravariant ( (>$<) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Foreign.Storable.Record as Store
import Foreign.Storable
import Foreign.Marshal.Utils
import CategAlgebra
import Hedgehog.Internal.Range (constantBounded)

foreign import capi "csrc/struct.h getfield" getfield :: Ptr Foo -> IO CInt
data Foo = Foo {fs , s :: CInt} deriving (Generic, Eq, Show, Typeable)
deriving instance Generic CInt
instance Arg CInt
instance Vary CInt where
  vary = (\(CInt n) -> n) >$< vary
instance Arg Foo

store :: Store.Dictionary Foo
store = Store.run $ 
  liftA2 Foo (Store.element fs) (Store.element s)

instance Storable Foo where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

class CategAlgebra ioObj => GetFieldSpec ioObj where
  foo :: (LogicAlgebra (ioObj Foo), Categorical Foo) =>
    ioObj Foo

  rint :: (LogicAlgebra (ioObj CInt), Categorical CInt) =>
    ioObj CInt

  getfieldM :: Morphism ioObj
  fstM :: Morphism ioObj

  getfieldSp :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt) => 
    (String, Prop (Hom ioObj))
  getfieldSp = ("GetFieldSpec", sp)
    where
      fooH = hom @ioObj foo rint
      sp = eql fooH (UU $ getfieldM @ioObj) (UU $ fstM @ioObj)

  getFieldSpec :: (LogicAlgebra (ioObj Foo), LogicAlgebra (ioObj CInt),
    Categorical Foo, Categorical CInt, LogicAlgebra (ioObj ()), Categorical ()) =>
    ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  getFieldSpec = (getfieldSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U foo, U rint]

instance GetFieldSpec NamedIOSet where
  foo = NamedIOSet "Struct Foo" fooGen fooCoGen
  rint = NamedIOSet "Int" (CInt <$> int32 constantBounded) vary
  getfieldM = IOMorphism $ \(foo' :: IO Foo) -> do
    foo <- foo'
    fooptr <- new foo
    getfield fooptr
  fstM = IOMorphism $ \(foo' :: IO Foo) -> fs <$> foo'

fooGen :: Gen Foo
fooGen = do
  let f1 = CInt <$> int32 constantBounded
      f2 = CInt <$> int32 constantBounded
    in Foo <$> f1 <*> f2

fooCoGen :: CoGen Foo
fooCoGen = (\(Foo f s) -> (f, s)) >$< vary

