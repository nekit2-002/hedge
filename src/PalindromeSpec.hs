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
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE CApiFFI #-}

module PalindromeSpec where
import CategAlgebra
import Data.Int
import Data.Typeable
import Foreign.C.String
import Foreign.C.Types
import GHC.Generics (Generic)
import Hedgehog.Function.Internal
import WriteReadSpec ()

foreign import capi "palindrome.h is_palindrome" isPalindrome :: CString -> CBool

-- isPalindrome :: String -> Bool
-- isPalindrome cs = cs == reverse cs

newtype Palindrome = P {p :: [CChar]} deriving (Typeable, Eq, Generic)
instance Show Palindrome where
  show (P p) = map castCCharToChar p
instance Arg Palindrome

class CategAlgebra obj => PalindromeSpec obj where
  pal :: (LogicAlgebra (obj Palindrome), Categorical Palindrome) =>
    obj Palindrome

  res :: (LogicAlgebra (obj Bool), Categorical Bool) =>
    obj Bool

  palM :: Morphism obj
  revPalM :: Morphism obj

  symEq :: (LogicAlgebra (obj Palindrome), Categorical Palindrome,
   LogicAlgebra (obj Bool), Categorical Bool) =>
    (String, Prop (Hom obj))
  symEq = ("Symmetry law", sp)
    where
      palHom = hom @obj pal res
      sp = eql palHom (UU $ palM @obj) (UU $ revPalM @obj)

  symEqSpec :: (LogicAlgebra (obj Palindrome), Categorical Palindrome,
   LogicAlgebra (obj Bool), Categorical Bool, LogicAlgebra (obj ()),
   Categorical ()) =>
   ([(String, Prop (Hom obj))], [EvalUnit obj])
  symEqSpec =  (symEq @obj : props, objs)
    where
      (props, objs) = categoryLaws [U pal, U res]
