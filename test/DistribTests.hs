{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}

module DistribTests where

import DistribSpec
import CategAlgebra ( EvalUnit(U), UU(UU) ) 
import CatLaws
import Hedgehog.Internal.Range (constantBounded)
import Hedgehog.Internal.Gen (integral) 
import Hedgehog.Function (Vary(vary))
import Data.Typeable ( cast )
-- import Data.Bifunctor ( Bifunctor(bimap) )
import Hedgehog.Internal.Runner (check)
import Data.Maybe ( fromJust )
import WriteReadTest (concatParams)
import Hedgehog.Internal.Property ( assert, property, withTests, Group(..), PropertyName(..))
import System.TimeIt (timeIt)

class DistribSpec obj => DistribTests obj where
  -- distribLaws :: Group
  distribLaws :: IO [Bool]

instance DistribSpec NamedSet where
  ints = NamedSet "Int" g vary
    where
      g = S <$> integral constantBounded

  lints = NamedSet "LongInt" g vary
    where 
      g = LS <$> integral constantBounded
  squareM (UU (a' :: t)) =
    let (S a) = fromJust $ cast @t @IntSet a'
      in Morphism $ \(S b) -> LS $ fromIntegral ((a * b)^2)
  squareMF x e = UU $ squareM @NamedSet (x e)
  distribSquare (UU (a' :: t)) =
    let (S a) = fromJust $ cast @t @IntSet a'
      in Morphism $ \(S b) -> LS $ fromIntegral (a^2 * b^2)
  distribSquareF x e = UU $ distribSquare @NamedSet (x e)
  toIntProp = id
  toHomProp = id

instance DistribTests NamedSet where
  -- distribLaws = Group "Laws of pow distribution for ints" $ map (bimap PropertyName (withTests 1000 . property)) int_tests
  --   where
  --     (specs, sets) = distribSquareSpec @NamedSet
  --     (distrib_spec, distp) = head specs
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
  --     int_tests = (distrib_spec ++ natParams, distp >>= assert) : category_tests
  distribLaws = (:) <$> (do
      putStrLn $ "\ESC[96m" ++ distrib_n
      timeIt . check . withTests 1000 . property $ p) <*>
    mapM (\(pn, p) -> do
    putStrLn $ "\ESC[96m" ++ pn
    timeIt . check . withTests 1000 . property $ p) category_tests
    where
      (specs, sets) = distribSquareSpec @NamedSet
      (distribSpec, distribp) = head specs
      cat_laws = tail specs
      comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
      comps2 = (,) <$> sets <*> sets
      compAssocParams =
        map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
        U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
        "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
      compIdParams = map (\(U (NamedSet n _ _ ), U (NamedSet n2 _ _)) -> 
        "(" ++ n ++ ", " ++ n2 ++ ")") comps2
      params = compAssocParams ++ compIdParams ++ compIdParams
      category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
      distrib_params' = tail sets
      distrib_params = concatParams $ map (\(U (NamedSet n _ _)) -> n) distrib_params'
      (distrib_n, p) = (distribSpec ++ distrib_params, distribp >>= assert)