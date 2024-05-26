{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module WriteReadTest where

import CategAlgebra (EvalUnit(U))
import Hedgehog.Internal.Runner (check)
-- import Data.Bifunctor (Bifunctor(bimap) )
import WriteReadSpec
    ( NamedIOSet(NamedIOSet), WriteReadSpec(writeReadSpec) )
import Hedgehog.Internal.Property (assert, property, withTests, Group(..), PropertyName(..))
import System.TimeIt (timeIt)

class WriteReadSpec ioObj => WriteReadTester ioObj where
  writeReadLaws :: IO [Bool]
  -- writeReadLaws :: Group

instance WriteReadTester NamedIOSet where
  -- writeReadLaws = Group "Read ∘ Write = id laws" $
  --   (PropertyName write_read_n, withTests 1000 $ property p):
  --   map (bimap PropertyName (withTests 1000 . property)) category_tests
  --   where
  --     (specs, sets) = writeReadSpec @NamedIOSet
  --     (write_read_spec, write_read_idp) = head specs
  --     cat_laws = tail specs
  --     comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
  --     comps2 = (,) <$> sets <*> sets
  --     compAssocParams =
  --       map (\(U (NamedIOSet n _ _), U (NamedIOSet n2 _ _),
  --       U (NamedIOSet n3 _ _), U (NamedIOSet n4 _ _) ) ->
  --       "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
  --     compIdParams = map (\(U (NamedIOSet n _ _ ), U (NamedIOSet n2 _ _)) -> 
  --       "(" ++ n ++ ", " ++ n2 ++ ")") comps2
  --     params = compAssocParams ++ compIdParams ++ compIdParams
  --     category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
  --     write_read_id_params' = tail sets
  --     write_read_id_params = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) write_read_id_params'
  --     (write_read_n, p) = (write_read_spec ++ write_read_id_params, write_read_idp >>= assert)
  writeReadLaws =
    (:) <$> (do
      putStrLn $ "\ESC[96m" ++ write_read_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = writeReadSpec @NamedIOSet
      (write_read_spec, write_read_idp) = head specs
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
      write_read_id_params' = tail sets
      write_read_id_params = concatParams $ map (\(U (NamedIOSet n _ _)) -> n) write_read_id_params'
      (write_read_n, p) = (write_read_spec ++ write_read_id_params, write_read_idp >>= assert)

concatParams' :: [String] -> String
concatParams' [p] = p ++ ")"
concatParams' (p:ps) = p ++ ", " ++ concatParams' ps

concatParams :: [String] -> [Char]
concatParams x = '(':concatParams' x
