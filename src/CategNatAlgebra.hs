module CategNatAlgebra where
import CategAlgebra
import Data.Int (Int32)
import Data.Typeable
import Hedgehog.Function
-- import Hedgehog.Function.Internal

newtype Nat = N {n :: Int32} deriving (Show, Eq, Generic, Typeable)

instance Arg Nat

instance Vary Nat where
  vary = contramap (\(N n) -> if 0 <= n then n else (-n)) vary

class CategAlgebra obj => CategNatAlgebra obj where
  nat :: (LogicAlgebra (obj Nat), Typeable Nat, Eq Nat, Show Nat, Arg Nat) =>
    obj Nat

  succM :: Morphism obj
  
  natSuccId :: (LogicAlgebra (obj Nat), Typeable Nat, Eq Nat, Show Nat, Arg Nat) =>
    (String, Prop (Hom obj))
  natSuccId = ("NatSuccId ", sp)
    where
      sp = eql (hom @obj nat nat) (UU $ succM @obj) (UU $ succM @obj)

  succIdSpec :: (LogicAlgebra (obj Nat), 
    Typeable Nat, Eq Nat, Show Nat, Arg Nat, LogicAlgebra (obj ()),
    Typeable (), Eq (), Show (), Arg ()) =>
   ([(String, Prop (Hom obj))], [EvalUnit obj])
  succIdSpec = (natSuccId @obj : props, objs)
    where
      (props, objs) = categoryLaws [U nat]