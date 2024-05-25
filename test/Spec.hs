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

import CategAlgebra
import CatLaws ( NamedSet(NamedSet) )
import NatTests (natLaws)
import WriteReadTest (writeReadLaws)
import Hedgehog ( assert, property, withTests, check, checkParallel )
import WriteReadSpec (NamedIOSet)
import PalindromeTests
import Data.Bifunctor ( Bifunctor(bimap) )
import System.TimeIt (timeItNamed, timeIt)
import Hedgehog.Internal.Property (Group(..), PropertyName(..))
import DistribTests (DistribTests(distribLaws))

class CategAlgebra obj => Tester obj where
  -- catLaws :: IO [Bool]
  catLaws :: Group


instance Tester NamedSet where
  catLaws = Group "Category laws" $ map (bimap PropertyName (withTests 300 . property)) tests
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
  -- catLaws =  mapM (\(pn, p) -> do
  --   putStrLn $ "\ESC[96m" ++ pn
  --   timeIt . check . withTests 300 . property $ p) tests
  --   where
  --     (specs, sets) = categoryLaws []
  --     comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
  --     comps2 = (,) <$> sets <*> sets
  --     compAssocParams =
  --       map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
  --       U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
  --       "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
  --     compIdParams = map (\(U (NamedSet n _ _), U (NamedSet n2 _ _)) -> 
  --       "(" ++ n ++ ", " ++ n2 ++ ")") comps2
  --     params = compAssocParams ++ compIdParams ++ compIdParams
  --     tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) specs params

main :: IO ()
main = do
  -- putStrLn $ "\ESC[93m" ++ "Category laws tests"
  -- _ <- timeItNamed "Category laws" $ catLaws @NamedSet
  _ <- timeItNamed "Category laws" . checkParallel $ catLaws @NamedSet
  -- putStrLn $ "\ESC[93m" ++ "Nat succ id tests"
  -- _ <- timeItNamed "Nat succ id tests" $ natLaws @NamedSet
  -- _ <- timeItNamed "Nat succ id tests" . checkParallel $ natLaws @NamedSet
  -- putStrLn $ "\ESC[93m" ++ "WriteReadId laws"
  -- _ <- timeItNamed "WriteReadId laws" $ writeReadLaws @NamedIOSet
  -- _ <- timeItNamed "WriteReadId" . checkParallel $ writeReadLaws @NamedIOSet
  -- putStrLn $ "\ESC[93m" ++ "Pow distribute laws"
  -- _ <- timeItNamed "Pow distribute laws" $ distribLaws @NamedSet
  -- _ <- timeItNamed "Square distribute laes" . checkParallel $ distribLaws @NamedSet

  -- _ <- timeItNamed "Pow distribute laws" . checkParallel $ distribLaws @NamedSet
  putStrLn $ "\ESC[93m" ++ "Palindrome reverse symmetry"
  _ <- timeItNamed "Palindrome reverse symmetry" $ palSymLaws @NamedIOSet
  -- _ <- timeItNamed "Palindrome reverse symmetry" . checkParallel $ palSymLaws @NamedIOSet
  
  pure ()