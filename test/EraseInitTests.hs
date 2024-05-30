{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module EraseInitTests where

import CategAlgebra
import WriteReadSpec
import EraseInit
import Hedgehog.Internal.Runner (check)
import Hedgehog.Internal.Property
import System.TimeIt (timeIt)

class EraseInitSpec ioObj => EraseInitTester ioObj where
  eraseTests :: IO [Bool]

instance EraseInitTester NamedIOSet where
  eraseTests = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ erase_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = eraseSpec @NamedIOSet
      (erases, erasep) = head specs
      cat_laws = tail specs
      (morph_n, mp) = head cat_laws
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
      (erase_n, p) = (erases ++ "(String, SepString)", erasep >>= assert)