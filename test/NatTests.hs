module NatTests where

import CategAlgebra
import CategNatAlgebra
import CatLaws
import Hedgehog.Internal.Range
import Hedgehog.Internal.Gen 



instance CategNatAlgebra NamedSet where
  nat = NamedSet "Nat" g cg (N 0)
    where
      g = N <$> integral (Hedgehog.Internal.Range.constant 0 maxBound)
      cg = undefined
  succM = Morphism (N 0) (\(N n) -> N (n + 1))