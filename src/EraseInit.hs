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


module EraseInit where
import WriteReadSpec
import Data.Typeable
import Data.Maybe
import CategAlgebra
import Hedgehog.Internal.Gen
import qualified Hedgehog.Internal.Range as Range
import Foreign.C

foreign import capi "csrc/erasesubstr.h erase_with_cspan" compSpan :: CString -> CString -> IO CString
foreign import capi "csrc/erasesubstr.h erase_with_strpbrk" pbrk :: CString -> CString -> IO CString

class CategAlgebra ioObj => EraseInitSpec ioObj where
  separators :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    ioObj Buf
  
  strings :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    ioObj Buf

  cspanEraseM :: UU -> Morphism ioObj
  cspanEraseMF :: UFormula (Hom ioObj) -> UFormula (Hom ioObj)
  pbrkM :: UU -> Morphism ioObj
  pbrkMF :: UFormula (Hom ioObj) -> UFormula (Hom ioObj)

  eraseSp :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    (String, Prop (Hom ioObj))
  eraseSp = ("EraseSpec", bPtoHp @ioObj $ ground @(ioObj Buf) sp [])
    where
      stringHom = hom @ioObj strings strings
      pick' = pick @(Hom ioObj)
      sp = forall' @(ioObj Buf) separators . hToB @ioObj .
        eqlF stringHom (pbrkMF @ioObj $ pick' 0) . cspanEraseMF @ioObj $ pick' 0
  
  hToB :: PFormula (Hom ioObj) -> PFormula (ioObj Buf)
  bPtoHp :: Prop (ioObj Buf) -> Prop (Hom ioObj)

  eraseSpec :: (LogicAlgebra (ioObj ()), Categorical (), LogicAlgebra (ioObj Buf),
    Categorical Buf) => ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  eraseSpec = (eraseSp @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U separators, U strings]

instance EraseInitSpec NamedIOSet where
  separators = NamedIOSet "SepString" sepGen bufCoGen
  strings = NamedIOSet "String" stringGen bufCoGen
  cspanEraseM (UU (seps' :: t))= IOMorphism $ \(str' :: IO Buf) -> do
    str <- str' >>= newCString . (\(PreCString b) -> Prelude.map castCCharToChar b)
    seps <-fromJust (cast @t @(IO Buf) seps') >>= newCString . (\(PreCString b) -> Prelude.map castCCharToChar b)
    res <- compSpan str seps >>= peekCString
    pure . PreCString $ Prelude.map castCharToCChar res
  cspanEraseMF x e = UU $ cspanEraseM @NamedIOSet (x e)
  pbrkM (UU (seps' :: t))= IOMorphism $ \(str' :: IO Buf) -> do
    str <- str' >>= newCString . (\(PreCString b) -> Prelude.map castCCharToChar b)
    seps <-fromJust (cast @t @(IO Buf) seps') >>= newCString . (\(PreCString b) -> Prelude.map castCCharToChar b)
    res <- pbrk str seps >>= peekCString
    pure . PreCString $ Prelude.map castCharToCChar res
  pbrkMF x e = UU $ pbrkM @NamedIOSet (x e)
  hToB = id
  bPtoHp = id

stringGen :: Gen Buf
stringGen = PreCString <$> (Prelude.map castCharToCChar <$> string (Range.constant 20 30) alphaNum)

sepGen :: Gen Buf
sepGen = PreCString <$> (Prelude.map castCharToCChar <$> string (Range.constant 1 10) digit)