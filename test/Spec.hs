import CategAlgebra
import CatLaws ( NamedSet(NamedSet) )
import NatTests (natLaws)
import WriteReadTest (writeReadLaws)
import Hedgehog ( assert, property, withTests, check, checkParallel )
import WriteReadSpec (NamedIOSet)
import PalindromeTests
import System.TimeIt (timeItNamed, timeIt)
import DistribTests (DistribTests(distribLaws))

class CategAlgebra obj => Tester obj where
  catLaws :: IO [Bool]

instance Tester NamedSet where
  catLaws =  mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 300 . property $ p) tests
    where
      (specs, sets) = categoryLaws []
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
        U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedSet n _ _), U (NamedSet n2 _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) specs params

main :: IO ()
main = do
  putStrLn $ "\ESC[93m" ++ "Category laws tests"
  _ <- timeItNamed "Category laws" $ catLaws @NamedSet
  -- _ <- checkParallel $ natLaws @NamedSet
  putStrLn $ "\ESC[93m" ++ "WriteReadId laws"
  _ <- timeItNamed "WriteReadId laws" $ writeReadLaws @NamedIOSet
  putStrLn $ "\ESC[93m" ++ "Pow distribute laws"
  _ <- timeItNamed "Pow distribute laws" $ distribLaws @NamedSet

  -- _ <- timeIt . checkParallel $ distribLaws @NamedSet
  putStrLn $ "\ESC[93m" ++ "Palindrome reverse symmetry"
  _ <- timeItNamed "Palindrome reverse symmetry" $ palSymLaws @NamedSet
  
  pure ()