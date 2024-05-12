module CategAlgebra where

import Data.Typeable
import Data.Kind

import Hedgehog.Function

data UU = forall t. (Eq t, Show t, Typeable t) => UU t deriving (Typeable)

class Universum s where
  type UFormula s
  quote :: UU -> UFormula s
  pick :: Int -> UFormula s

class Universum s => Determinable s where
  type Prop s
  contain :: s -> UU -> Prop s
  eql :: s -> UU -> UU -> Prop s

class Determinable s => LogicAlgebra s where
  type PFormula s
  ground :: PFormula s -> [UU] -> Prop s
  elemF :: s -> UFormula s -> PFormula s
  eqlF :: s -> UFormula s -> UFormula s -> PFormula s
  forall' :: s -> PFormula s -> PFormula s

  eqReflSpec :: s -> Prop s
  eqReflSpec s = ground @s (forall' @s s $ eqlF @s s (pick @s 0) (pick @s 0)) []

data EvalUnit (obj :: Type -> Type) =
  forall t. (Eq t, Show t, Typeable t, Arg t, LogicAlgebra (obj t)) => U (obj t)
class (LogicAlgebra (Hom obj), Typeable (Morphism obj), Eq (Morphism obj), Show (Morphism obj)) => 
  CategAlgebra (obj :: Type -> Type) where
  type Morphism obj
  type Hom obj

  hom :: (LogicAlgebra (obj a), LogicAlgebra (obj b), Typeable b,
    Typeable a, Eq b, Eq a, Show b, Show a, Arg a, Arg b) =>
    obj a -> obj b -> Hom obj

  comp :: Morphism obj -> Morphism obj -> Morphism obj
  compF :: UFormula (Hom obj) -> UFormula (Hom obj) -> UFormula (Hom obj)

  compAssSpec :: (LogicAlgebra (obj a), LogicAlgebra (obj b),
    LogicAlgebra (obj c), LogicAlgebra (obj d), Show a, Eq a, Typeable a,
    Arg a, Show b, Eq b, Typeable b, Arg b, Show c, Eq c, Typeable c,
    Arg c, Eq d, Show d, Typeable d, Arg d) =>
    obj a -> obj b -> obj c -> obj d -> (String, Prop (Hom obj))
  compAssSpec (a :: obj a) (b :: obj b) (c :: obj c) (d :: obj d) = ("compAssSpec", ground @(Hom obj) sp [])
    where
      f = hom a b
      g = hom b c
      h = hom c d
      aToD = hom a d
      compF' = compF @obj
      pick' = pick @(Hom obj)
      sp = forall' h . forall' g . forall' f .
        eqlF aToD (compF' (pick' 0) . compF' (pick' 1) $ pick' 2) .
          compF' (compF' (pick' 0) $ pick' 1) $ pick' 2

  idm :: (LogicAlgebra (obj a),Typeable a, Eq a, Show a, Arg a)=>
    obj a -> Morphism obj

  idLSpec :: (LogicAlgebra (obj a), LogicAlgebra (obj b),
    Typeable a, Eq a, Show a, Arg a,
    Typeable b, Eq b, Show b, Arg b) =>
    obj a -> obj b -> (String, Prop (Hom obj))
  idLSpec (a :: obj a) (b :: obj b) = ("idLSpec", ground @(Hom obj) sp [])
    where
      f = hom a b
      pick' = pick @(Hom obj)
      compF' = compF @obj
      sp = forall' f $
        eqlF f (compF' (pick' 0) (quote @(Hom obj) $ UU (idm b))) $ pick' 0

  idRSpec :: (LogicAlgebra (obj a), LogicAlgebra (obj b),
    Typeable a, Eq a, Show a, Arg a,
    Typeable b, Eq b, Show b, Arg b) =>
    obj a -> obj b -> (String, Prop (Hom obj))
  idRSpec (a :: obj a) (b :: obj b) = ("idRSpec", ground @(Hom obj) sp [])
    where
      f = hom a b
      pick' = pick @(Hom obj)
      compF' = compF @obj
      sp = forall' f $
        eqlF f (compF' (quote @(Hom obj) $ UU (idm a)) (pick' 0)) $ pick' 0

  unitO :: (LogicAlgebra (obj ()), Typeable (), Eq (), Show (), Arg ()) => obj ()
    
  categoryLaws :: (LogicAlgebra (obj ()), Typeable (), Eq (), Show (), Arg ()) =>
    [EvalUnit obj] -> ([(String, Prop (Hom obj))], [EvalUnit obj])
  categoryLaws [] = ([compAssSpec @obj unitO unitO unitO unitO, idLSpec @obj unitO unitO, idRSpec @obj unitO unitO], [U unitO])
  categoryLaws objs' = (compAssoc ++ idL ++ idR, objs)
    where
      objs = U unitO : objs' 
      allSets4 = (,,,) <$> objs <*> objs <*> objs <*> objs
      compAssoc = map (\(U a, U b, U c, U d) -> compAssSpec a b c d) allSets4
      allSets2 = (,) <$> objs <*> objs
      idL = map (\(U a, U b) -> idLSpec a b) allSets2
      idR = map (\(U a, U b) -> idRSpec a b) allSets2