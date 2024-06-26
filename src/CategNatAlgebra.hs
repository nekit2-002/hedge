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
{-#LANGUAGE ConstrainedClassMethods #-}
{-#LANGUAGE ImpredicativeTypes #-}
{-#LANGUAGE CApiFFI #-}



module CategNatAlgebra where
import CategAlgebra
import Data.Int (Int32)
import Data.Typeable (Typeable)
import Hedgehog.Function

newtype Nat = N {n :: Int32} deriving (Show, Eq, Generic, Typeable)

instance Arg Nat

instance Vary Nat where
  vary = contramap (\(N n) -> abs n) vary

class CategAlgebra obj => CategNatAlgebra obj where
  nat :: (LogicAlgebra (obj Nat), Categorical Nat) =>
    obj Nat

  succM :: Morphism obj
  
  natSuccId :: (LogicAlgebra (obj Nat), Categorical Nat) =>
    (String, Prop (Hom obj))
  natSuccId = ("NatSuccId ", sp)
    where
      sp = eql (hom @obj nat nat) (UU $ succM @obj) (UU $ succM @obj)

  succIdSpec :: (LogicAlgebra (obj Nat), 
    Categorical Nat, LogicAlgebra (obj ()),
    Categorical ()) =>
   ([(String, Prop (Hom obj))], [EvalUnit obj])
  succIdSpec = (natSuccId @obj : props, objs)
    where
      (props, objs) = categoryLaws [U nat]