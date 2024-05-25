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

module NatTests where
-- ojgoig

import CategAlgebra (EvalUnit(U))
import CategNatAlgebra (Nat(N), CategNatAlgebra(succIdSpec, nat, succM) )
import CatLaws (MorphismImpl(Morphism), NamedSet(NamedSet))
import Hedgehog.Internal.Range (constant)
import Hedgehog.Internal.Gen (integral) 
import Hedgehog.Function (Vary(vary))
import Data.Bifunctor (bimap)
import System.TimeIt ( timeIt )
import Hedgehog.Internal.Runner ( check )
import Hedgehog.Internal.Property ( assert, property, withTests, Group(..),PropertyName(..))

instance CategNatAlgebra NamedSet where
  nat = NamedSet "Nat" g vary
    where
      g = N <$> integral (Hedgehog.Internal.Range.constant 0 maxBound)
  succM = Morphism (\(N n) -> N (n + 1))


class CategNatAlgebra obj => NatTester obj where
  -- natLaws :: IO [Bool]
  natLaws :: Group

instance NatTester NamedSet where
  natLaws = Group "Laws for natural ints (int32, natSuccId spec)" $ map (bimap PropertyName (withTests 1000 . property)) nat_tests
    where
      (specs, sets) = succIdSpec @NamedSet
      (succ_id_spec, succidp) = head specs
      cat_laws = tail specs
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
        U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedSet n _ _), U (NamedSet n2 _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
      natParams = (\(U (NamedSet n _ _)) -> "(" ++ n ++ ", " ++ n ++ ")") (last sets)
      nat_tests = (succ_id_spec ++ natParams, succidp >>= assert) : category_tests
  -- natLaws = mapM (\(pn, p) -> do
  --   putStrLn $ "\ESC[96m" ++ pn
  --   timeIt . check . withTests 1000 . property $ p) nat_tests
  --   where
  --     (specs, sets) = succIdSpec @NamedSet
  --     (succ_id_spec, succidp) = head specs
  --     cat_laws = tail specs
  --     comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
  --     comps2 = (,) <$> sets <*> sets
  --     compAssocParams =
  --       map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
  --       U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
  --       "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
  --     compIdParams = map (\(U (NamedSet n _ _), U (NamedSet n2 _ _)) -> 
  --       "(" ++ n ++ ", " ++ n2 ++ ")") comps2
  --     params = compAssocParams ++ compIdParams ++ compIdParams
  --     category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
  --     natParams = (\(U (NamedSet n _ _)) -> "(" ++ n ++ ", " ++ n ++ ")") (last sets)
  --     nat_tests = (succ_id_spec ++ natParams, succidp >>= assert) : category_tests