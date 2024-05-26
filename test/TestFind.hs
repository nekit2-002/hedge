{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module TestFind where
import Hedgehog.Internal.Runner (check)
import Data.Functor.Contravariant
import CategAlgebra
import System.TimeIt (timeIt)
import WriteReadSpec (NamedIOSet(..))
import WriteReadTest (concatParams)
import Hedgehog.Internal.Property (assert, property, withTests)
import FindSpec

class FindSpec ioObj => FindTester ioObj where
  findTests :: IO [Bool]

instance FindTester NamedIOSet where
  findTests = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ transform_n
      timeIt . check . withTests 250000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = transrormIdSpec @NamedIOSet
      (transform_spec, transformp) = head specs
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
      transform' = tail sets
      transform = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) transform'
      (transform_n, p) = (transform_spec ++ transform, transformp >>= assert)
