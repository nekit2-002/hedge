{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module GetFieldTests where

import CategAlgebra
import WriteReadSpec
import WriteReadTest (concatParams)
import GetFieldSpec
import Hedgehog.Internal.Runner (check)
import Hedgehog.Internal.Property
import System.TimeIt (timeIt)

class GetFieldSpec ioObj => GetFieldTester ioObj where
  getFieldTests :: IO [Bool]

instance GetFieldTester NamedIOSet where
  getFieldTests = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ getN_n
      timeIt . check . withTests 500 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = getFieldSpec @NamedIOSet
      (getNSpec, getnp) = head specs
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
      getNparams' = tail sets
      getNparams = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) getNparams'
      (getN_n, p) = (getNSpec ++ "(Struct Foo)", getnp >>= assert)