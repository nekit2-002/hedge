module NatTests where
-- ojgoig

import CategAlgebra (EvalUnit(U))
import CategNatAlgebra (Nat(N), CategNatAlgebra(succIdSpec, nat, succM) )
import CatLaws (MorphismImpl(Morphism), NamedSet(NamedSet))
import Hedgehog.Internal.Range (constant)
import Hedgehog.Internal.Gen (integral) 
import Hedgehog.Function (Vary(vary))
import Data.Bifunctor (bimap)
import Hedgehog.Internal.Property

instance CategNatAlgebra NamedSet where
  nat = NamedSet "Nat" g vary (N 0)
    where
      g = N <$> integral (Hedgehog.Internal.Range.constant 0 maxBound)
  succM = Morphism (N 0) (\(N n) -> N (n + 1))


class CategNatAlgebra obj => NatTester obj where
  natLaws :: Group

instance NatTester NamedSet where
  natLaws = Group "Laws for natural ints (int32, natSuccId spec)" $ map (bimap PropertyName (withTests 2000 . property)) nat_tests
    where
      (specs, sets) = succIdSpec @NamedSet
      (succ_id_spec, succidp) = head specs
      cat_laws = tail specs
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedSet n _ _ _), U (NamedSet n2 _ _ _),
        U (NamedSet n3 _ _ _), U (NamedSet n4 _ _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedSet n _ _ _), U (NamedSet n2 _ _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
      natParams = (\(U (NamedSet n _ _ _)) -> "(" ++ n ++ ", " ++ n ++ ")") (last sets)
      nat_tests = (succ_id_spec ++ natParams, succidp >>= assert) : category_tests