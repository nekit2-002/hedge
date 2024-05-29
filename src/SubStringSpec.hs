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
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE CApiFFI #-}

module SubStringSpec where

import CategAlgebra
import Data.Typeable ( Typeable )
import Foreign.C.String ( castCCharToChar, CString, newCString, castCharToCChar )
import Foreign.C.Types
import Data.Functor.Contravariant ((>$<))
import Data.Int
import Data.Typeable
import Foreign.Ptr(Ptr(..))
import GetFieldSpec()
import Hedgehog.Internal.Gen(int32, Gen, string, lower)
import Hedgehog.Internal.Range (constant)
import GHC.Generics (Generic)
import Hedgehog.Function.Internal 
import WriteReadSpec 
import Data.Maybe (fromJust)

foreign import capi "csrc/substring.h substrpos" subStrPos :: CString -> CString -> IO CInt
foreign import capi "csrc/substring.h copysubstrpos" copySubStrPos :: CString -> CString -> IO CInt

class CategAlgebra ioObj => SubStringSpec ioObj where
  pos :: (LogicAlgebra (ioObj Int32), Categorical Int32) =>
    ioObj Int32
  str' :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    ioObj Buf
  substr :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    ioObj Buf

  subStrM :: UU -> Morphism ioObj
  subStrMF :: UFormula (Hom ioObj) -> UFormula (Hom ioObj)
  subStrCpM :: UU -> Morphism ioObj
  subStrCpMF :: UFormula (Hom ioObj) -> UFormula (Hom ioObj)


  subStrSp :: (LogicAlgebra (ioObj Int32), Categorical Int32,
    LogicAlgebra (ioObj Buf), Categorical Buf) =>
    (String, Prop (Hom ioObj))
  subStrSp = ("SubStringSpec", bufToHom @ioObj $ ground @(ioObj Buf) sp [])
    where
      bhom = hom @ioObj str' pos
      pick' = pick @(Hom ioObj)
      sp = forall' @(ioObj Buf) substr . homToBuf @ioObj .
        eqlF bhom (subStrMF @ioObj (pick' 0)) $ subStrCpMF @ioObj (pick' 0)

  homToBuf :: PFormula (Hom ioObj) -> PFormula (ioObj Buf)
  bufToHom :: Prop (ioObj Buf) -> Prop (Hom ioObj)

  subStrSpec :: (LogicAlgebra (ioObj Buf), Categorical Buf, LogicAlgebra (ioObj ()),
    Categorical (), LogicAlgebra (ioObj Int32), Categorical Int32) =>
    ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  subStrSpec = (subStrSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U pos, U str', U substr]
  
instance SubStringSpec NamedIOSet where
  pos = NamedIOSet "Int" posGen ((\n -> if n >= (-1) then n else abs n) >$<vary)
  str' = NamedIOSet "String" strGen strCoGen
  substr = NamedIOSet "Substring" substrGen substrCoGen
  subStrM (UU (substr' :: t)) = IOMorphism $ \(str' ::IO Buf) -> do
    str <- str' >>= newCString . (\(PreCString s) -> map castCCharToChar s)
    substr <- fromJust (cast @t @(IO Buf) substr') >>= newCString . (\(PreCString s) -> map castCCharToChar s)
    subStrPos str substr

  subStrMF x e = UU $ subStrM @NamedIOSet (x e)
  subStrCpM (UU (substr' :: t))= IOMorphism $ \(str' :: IO Buf) -> do
    str <- str' >>= newCString . (\(PreCString s) -> map castCCharToChar s)
    substr <- fromJust (cast @t @(IO Buf) substr') >>= newCString . (\(PreCString s) -> map castCCharToChar s)
    copySubStrPos str substr
  
  subStrCpMF x e = UU $ subStrCpM @NamedIOSet (x e)
  homToBuf = id
  bufToHom = id


posGen :: Gen Int32
posGen = int32 (constant (-1) 50)

strGen :: Gen Buf
strGen = PreCString . map castCharToCChar <$> string (constant 1 50) lower

strCoGen :: CoGen Buf
strCoGen = go >$< (vary :: CoGen [Int8])
  where
    go (PreCString cs') =
      let pres = map castCCharToChar cs'
        in map ((\(CChar n) -> n) . castCharToCChar) pres

substrGen :: Gen Buf
substrGen = PreCString . map castCharToCChar <$> string (constant 1 5) lower

substrCoGen :: CoGen Buf
substrCoGen = go >$< (vary :: CoGen [Int8])
  where
    go (PreCString cs') =
      let pres = take 5 $ map castCCharToChar cs'
        in map ((\(CChar n) -> n) . castCharToCChar) pres