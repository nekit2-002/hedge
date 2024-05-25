{-#LANGUAGE BlockArguments #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE NoStarIsType #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeFamilyDependencies #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE CApiFFI #-}
-- {-#LANGUAGE DatatypeContexts#-}

module DistribSpec where

import CategAlgebra
import Data.Int (Int32, Int64)
import Data.Typeable (Typeable)
import Hedgehog.Function

newtype IntSet = S {n :: Int32} deriving (Show, Eq, Generic, Typeable)
instance Arg IntSet
instance Vary IntSet where
  vary = contramap (\(S n) -> n) vary

newtype LongSet = LS {n1 :: Int64} deriving (Show, Eq, Generic, Typeable)
instance Arg LongSet
instance Vary LongSet where
  vary = contramap (\(LS n) -> n) vary


class CategAlgebra obj => DistribSpec obj where
  ints :: (LogicAlgebra (obj IntSet), Categorical IntSet) =>
    obj IntSet
  
  lints :: (LogicAlgebra (obj LongSet), Categorical LongSet) =>
    obj LongSet

  squareM :: UU -> Morphism obj
  squareMF :: UFormula (Hom obj) -> UFormula (Hom obj)
  distribSquare :: UU -> Morphism obj
  distribSquareF :: UFormula (Hom obj) -> UFormula (Hom obj)

  distribSquareS :: (LogicAlgebra (obj IntSet), Categorical IntSet,
    LogicAlgebra (obj LongSet), Categorical LongSet) =>
   (String, Prop (Hom obj))
  distribSquareS = ("DistributeSquareLaw", toHomProp @obj $ ground @(obj IntSet) sp [])
    where
      intHom = hom @obj ints lints
      pick' = pick @(Hom obj)
      sp = forall' @(obj IntSet) ints . toIntProp @obj $
        eqlF intHom (squareMF @obj $ pick' 0) (distribSquareF @obj $ pick' 0)

  toIntProp :: PFormula (Hom obj) -> PFormula (obj IntSet)
  toHomProp :: Prop (obj IntSet) -> Prop (Hom obj)

  distribSquareSpec :: (LogicAlgebra (obj IntSet), LogicAlgebra (obj ()),
    Categorical IntSet, Categorical (), LogicAlgebra (obj LongSet), Categorical LongSet) =>
    ([(String, Prop (Hom obj))], [EvalUnit obj])
  distribSquareSpec = (distribSquareS @obj : props, objs)
    where
      (props, objs) = categoryLaws [U ints]



