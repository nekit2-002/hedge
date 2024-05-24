module PalindromeTests where

import Data.Typeable
-- import Data.Bifunctor
import Hedgehog.Internal.Runner (check)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Word
import CategAlgebra
import WriteReadTest (concatParams)
import Hedgehog.Internal.Property
import Hedgehog.Internal.Gen (bool, Gen, string, alpha)
import Hedgehog.Function.Internal
import Hedgehog.Internal.Range (constant)
import Data.Int (Int8)
import System.TimeIt (timeIt)
import PalindromeSpec
import Data.Functor.Contravariant ((>$<))
import CatLaws
import Data.ByteString.Internal (c2w, w2c)


class PalindromeSpec obj => PalindromeTest obj where
  -- palSymLaws :: IO [Bool]
  palSymLaws :: Group

instance PalindromeSpec NamedSet where
  pal = NamedSet "PalindromeSet" palindromeGen palindromCoGen
  res = NamedSet "Bool" bool (vary :: CoGen Bool)

  palM = Morphism $ \(P prebuf) ->
    let buf = map (w2c . fromIntegral @Int8 @Word8) prebuf
      in isPalindrome buf
  revPalM = Morphism $ \(P prebuf) ->
    let  buf = map (w2c . fromIntegral @Int8 @Word8) prebuf
      in isPalindrome $ reverse buf

instance PalindromeTest NamedSet where
  palSymLaws = Group "Palindrom symmetry" $ map (bimap PropertyName (withTests 1000 . property)) sym_tests
    where
      (specs, sets) = symEqSpec @NamedSet
      (symSpec, symp) = head specs
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
      symEqParams' = tail sets
      symEqParams = concatParams $ map (\(U (NamedSet n _ _)) -> n) symEqParams'
      sym_tests = (symSpec ++ symEqParams, symp >>= assert) : category_tests

  -- palSymLaws = (:) <$> (do
  --     putStrLn $ "\ESC[96m" ++ symEq_n
  --     timeIt . check . withTests 1000 . property $ p) <*>
  --   mapM (\(pn, p) -> do
  --   putStrLn $ "\ESC[96m" ++ pn
  --   timeIt . check . withTests 1000 . property $ p) category_tests
  --   where
  --     (specs, sets) = symEqSpec @NamedSet
  --     (symSpec, symp) = head specs
  --     cat_laws = tail specs
  --     comps4 = (,,,) <$> sets <*> sets <*> sets <*> sets
  --     comps2 = (,) <$> sets <*> sets
  --     compAssocParams =
  --       map (\(U (NamedSet n _ _), U (NamedSet n2 _ _),
  --       U (NamedSet n3 _ _), U (NamedSet n4 _ _) ) ->
  --       "(" ++ n ++ ", " ++ n2 ++ ", " ++ n3 ++ ", " ++ n4 ++ ")") comps4
  --     compIdParams = map (\(U (NamedSet n _ _ ), U (NamedSet n2 _ _)) -> 
  --       "(" ++ n ++ ", " ++ n2 ++ ")") comps2
  --     params = compAssocParams ++ compIdParams ++ compIdParams
  --     category_tests = zipWith (\(n, prop) ps -> (n ++ ps, prop >>= assert)) cat_laws params
  --     symEqParams' = tail sets
  --     symEqParams = concatParams $ map (\(U (NamedSet n _ _)) -> n) symEqParams'
  --     (symEq_n, p) = (symSpec ++ symEqParams, symp >>= assert)


palindromeGen :: Gen Palindrome
palindromeGen = P . (\s -> map (fromIntegral @Word8 @Int8 . c2w) $ s ++ reverse s) <$> string (constant 1 6) alpha

palindromCoGen :: CoGen Palindrome
palindromCoGen = go >$< (vary  :: CoGen [Int8])
  where
    go (P p) = 
      let buf' = map (w2c . fromIntegral @Int8 @Word8) p
          buf = buf' ++ reverse buf'
        in map (fromIntegral @Word8 @Int8 . c2w) buf