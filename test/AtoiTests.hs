{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module AtoiTests where

import CategAlgebra
import WriteReadSpec
import WriteReadTest (concatParams)
import Hedgehog.Function.Internal
import Hedgehog.Internal.Gen (string, Gen, digit, int)
import Hedgehog.Internal.Range (constant)
import Foreign.C (castCCharToChar, newCString, castCharToCChar)
import Foreign.C.Types
import Data.Functor.Contravariant
import Data.Char
import AtoiSpec
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import Hedgehog.Internal.Property (assert, property, withTests, Group(..), PropertyName(..))
import Data.Int (Int32, Int8)
import Data.Functor
import Hedgehog.Internal.Runner (check)
import System.TimeIt (timeIt)

instance AtoiSpec NamedIOSet where
  nums = NamedIOSet "NumString" numSGen numSCogen
  readNM = IOMorphism $ \(ns' :: IO NumS) -> do
    (NS ns) <- ns'
    let s = map castCCharToChar ns
      in pure $ Prelude.read @Integer s

  atoiM = IOMorphism $ \(ns' :: IO NumS) -> do
    (NS ns) <- ns'
    let s = map castCCharToChar ns
    cs <- newCString s
    (CInt n) <- atoi cs
    _ <- free cs
    pure $ fromIntegral @Int32 @Integer n
    

numSGen :: Gen NumS
numSGen = do
  NS . map castCharToCChar <$> string (constant 1 10) digit

numSCogen :: CoGen NumS
numSCogen = go >$< (vary :: (CoGen [Int8]))
  where
    go (NS cs) = 
      let pres = replaceWithDigit $ map castCCharToChar cs
        in map ((\(CChar n) -> n) . castCharToCChar) pres

replaceWithDigit :: String -> String
replaceWithDigit [] = []
replaceWithDigit (c:cs) = if isDigit c then c : replaceWithDigit cs else '1': replaceWithDigit cs

class AtoiSpec ioObj => AtoiTester ioObj where
  atoiTests :: IO [Bool]

instance AtoiTester NamedIOSet where
  atoiTests = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ atoi_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = atoiReadSpec @NamedIOSet
      (atoi_spec, atoip) = head specs
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
      atoi_params' = tail sets
      atoi_params = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) atoi_params'
      (atoi_n, p) = (atoi_spec ++ atoi_params, atoip >>= assert)