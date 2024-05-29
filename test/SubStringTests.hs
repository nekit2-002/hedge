{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module SubStringTests where
import CategAlgebra
import WriteReadSpec
import WriteReadTest (concatParams)
import SubStringSpec
import Hedgehog.Internal.Runner (check)
import Hedgehog.Internal.Property
import System.TimeIt (timeIt)

class SubStringSpec ioObj => SubStringTester ioObj where
  subStringTests :: IO [Bool]

instance SubStringTester NamedIOSet where
  subStringTests = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ subStr_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = subStrSpec @NamedIOSet
      (substr_spec, substrp) = head specs
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
      (subStr_n, p) = (substr_spec ++ "(Buffer)", substrp >>= assert)