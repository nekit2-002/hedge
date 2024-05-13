import CategAlgebra
import CatLaws
import NatTests
import Data.Bifunctor (bimap)
import Hedgehog
import Hedgehog.Internal.Property

class CategAlgebra obj => Tester obj where
  catLaws :: Group

instance Tester NamedSet where
  catLaws = Group "Category laws" $ map (bimap PropertyName (withTests 200 . property)) tests
    where
      (specs, sets) = categoryLaws []
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedSet n _ _ _), U (NamedSet n2 _ _ _),
        U (NamedSet n3 _ _ _), U (NamedSet n4 _ _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedSet n _ _ _), U (NamedSet n2 _ _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) specs params

main :: IO ()
main = do
  _ <- checkParallel $ catLaws @NamedSet
  _ <- checkParallel $ natLaws @NamedSet
  pure ()