{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE ImportQualifiedPost #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE NoStarIsType #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TypeFamilyDependencies #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedRecordDot #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE ImplicitParams #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE CApiFFI #-}
{-#LANGUAGE StandaloneDeriving #-}
-- {-#LANGUAGE DatatypeContexts#-}

module PalindromeTests where

import Data.Typeable
-- import Data.Bifunctor
import Hedgehog.Internal.Runner (check)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Word
import CategAlgebra
import WriteReadTest (concatParams)
import Hedgehog.Internal.Property
import Hedgehog.Internal.Gen (bool, Gen, string, alpha)
import Hedgehog.Function.Internal
import Hedgehog.Internal.Range (constant)
import Data.Int (Int8)
import System.TimeIt (timeIt)
import PalindromeSpec
import Data.Functor.Contravariant ((>$<))
import CatLaws
import Foreign.C.Types
import Foreign.C (castCharToCChar, castCCharToChar, newCString)
import WriteReadSpec

class PalindromeSpec ioObj => PalindromeTest ioObj where
  palSymLaws :: IO [Bool]
  -- palSymLaws :: Group

instance PalindromeSpec NamedIOSet where
  pal = NamedIOSet "PalindromeSet" palindromeGen palindromCoGen
  res = NamedIOSet "Bool" bool (vary :: CoGen Bool)

  palM = IOMorphism $ \(p' :: IO Palindrome) -> do
    (P p'') <- p'
    let buf = map castCCharToChar p''
    b <- newCString buf
    (CBool n') <- pure $ isPalindrome b
    n <- pure $ toInteger n'
    -- _ <- free buf
    if n == 1 then pure True else pure False
    
  revPalM = IOMorphism $ \(p' :: IO Palindrome) -> do
    (P p'') <- p'
    let buf = map castCCharToChar p''
    b <- newCString (reverse buf)
    (CBool n') <- pure $ isPalindrome b
    n <- pure $ toInteger n'
    -- _ <- free buf
    if n == 1 then pure True else pure False
    

instance PalindromeTest NamedIOSet where
  -- palSymLaws = Group "Palindrom symmetry" $ map (bimap PropertyName (withTests 1000 . property)) sym_tests
  --   where
  --     (specs, sets) = symEqSpec @NamedIOSet
  --     (symSpec, symp) = head specs
  --     cat_laws = tail specs
  --     comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
  --     comps2 = (,) <$> sets <*> sets
  --     compAssocParams =
  --       map (\(U (NamedIOSet n _ _), U (NamedIOSet n2 _ _),
  --       U (NamedIOSet n3 _ _), U (NamedIOSet n4 _ _) ) ->
  --       "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
  --     compIdParams = map (\(U (NamedIOSet n _ _ ), U (NamedIOSet n2 _ _)) -> 
  --       "(" ++ n ++ ", " ++ n2 ++ ")") comps2
  --     params = compAssocParams ++ compIdParams ++ compIdParams
  --     category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
  --     symEqParams' = tail sets
  --     symEqParams = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) symEqParams'
  --     sym_tests = (symSpec ++ symEqParams, symp >>= assert) : category_tests

  palSymLaws = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ symEq_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = symEqSpec @NamedIOSet
      (symSpec, symp) = head specs
      cat_laws = tail specs
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedIOSet n _ _), U (NamedIOSet n2 _ _),
        U (NamedIOSet n3 _ _), U (NamedIOSet n4 _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedIOSet n _ _ ), U (NamedIOSet n2 _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
      symEqParams' = tail sets
      symEqParams = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) symEqParams'
      (symEq_n, p) = (symSpec ++ symEqParams, symp >>= assert)


palindromeGen :: Gen Palindrome
palindromeGen = P . (\s -> map castCharToCChar $ s ++ reverse s) <$> string (constant 1 6) alpha

palindromCoGen :: CoGen Palindrome
palindromCoGen = go >$< (vary  :: CoGen [Int8])
  where
    go (P p') =
      let prep = map castCCharToChar p'
          p = prep ++ reverse prep
        in map ((\(CChar n) -> n) . castCharToCChar) p