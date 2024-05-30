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

module PalindromeSpec where
import CategAlgebra
import Data.Typeable ( Typeable )
import Foreign.C.String
import Foreign.C.Types ( CChar(..) )
import Data.Functor.Contravariant ( (>$<) )
import Data.Int ( Int8 )
import Hedgehog.Internal.Gen ( Gen, alpha, string )
import qualified Hedgehog.Internal.Range as Range
import GHC.Generics (Generic)
import Hedgehog.Function.Internal ( Arg, CoGen, Vary(vary) ) 
import WriteReadSpec 

foreign import capi "csrc/palindrome.h copy_palindrome" copyPalindrome :: CString -> IO CString
foreign import capi "csrc/palindrome.h reverse" palReverse :: CString -> IO CString

-- detectPalindrome :: String -> Bool
-- detectPalindrome cs = cs == reverse cs

newtype Palindrome = P {p :: [CChar]} deriving (Typeable, Eq, Generic)
instance Show Palindrome where
  show (P p) = Prelude.map castCCharToChar p
instance Arg Palindrome

class CategAlgebra ioObj => PalindromeSpec ioObj where
  pal :: (LogicAlgebra (ioObj Palindrome), Categorical Palindrome) =>
    ioObj Palindrome

  copyPalM :: Morphism ioObj
  revPalM :: Morphism ioObj

  symEq :: (LogicAlgebra (ioObj Palindrome), Categorical Palindrome,
   LogicAlgebra (ioObj Bool), Categorical Bool) =>
    (String, Prop (Hom ioObj))
  symEq = ("Symmetry law", sp)
    where
      palHom = hom @ioObj pal pal
      comp' = comp @ioObj
      sp = eql palHom (UU (idm @ioObj pal)) (UU $ comp' (copyPalM @ioObj) (revPalM @ioObj))

  symEqSpec :: (LogicAlgebra (ioObj Palindrome), Categorical Palindrome,
   LogicAlgebra (ioObj Bool), Categorical Bool, LogicAlgebra (ioObj ()),
   Categorical ()) =>
   ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  symEqSpec =  (symEq @ioObj : props, ioObjs)
    where
      (props, ioObjs) = categoryLaws [U pal]

instance PalindromeSpec NamedIOSet where
  pal = NamedIOSet "PalindromeSet" palindromeGen palindromCoGen

  copyPalM = IOMorphism $ \(p' :: IO Palindrome) -> do
    (P p'') <- p'
    let buf = Prelude.map castCCharToChar p''
    b <- newCString (reverse buf)
    s <- copyPalindrome b >>= peekCString
    pure . P $ Prelude.map castCharToCChar s

  revPalM = IOMorphism $ \(p' :: IO Palindrome) -> do
    (P p'') <- p'
    let buf = Prelude.map castCCharToChar p''
    b <- newCString (reverse buf)
    s <- palReverse b >>= peekCString
    pure . P $ Prelude.map castCharToCChar s

palindromeGen :: Gen Palindrome
palindromeGen = P . (\s -> Prelude.map castCharToCChar $ s ++ reverse s) <$> string (Range.constant 1 6) alpha

palindromCoGen :: CoGen Palindrome
palindromCoGen = go >$< (vary  :: CoGen [Int8])
  where
    go (P p') =
      let prep = Prelude.map castCCharToChar p'
          p = prep ++ reverse prep
        in Prelude.map ((\(CChar n) -> n) . castCharToCChar) p