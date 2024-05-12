import CategAlgebra

import Data.Int (Int32)
import Data.Typeable
import Hedgehog.Function

class CategAlgebra obj => CategNatAlgebra obj where
  nat :: (LogicAlgebra (obj Int32), Typeable Int32, Eq Int32, Show Int32, Arg Int32) =>
    obj Int32

  succM :: Morphism obj
  natSuccId :: (LogicAlgebra (obj Int32), Typeable Int32, Eq Int32, Show Int32, Arg Int32) =>
    (String, Prop (Hom obj))
  natSuccId = ("Nat succ id ", sp)
    where
      sp = eql (hom @obj nat nat) (UU $ succM @obj) (UU $ succM @obj)

  succIdSpec :: (LogicAlgebra (obj Int32), 
    Typeable Int32, Eq Int32, Show Int32, Arg Int32, LogicAlgebra (obj ()),
    Typeable (), Eq (), Show (), Arg ()) =>
   ([(String, Prop (Hom obj))], [EvalUnit obj])
  succIdSpec = (natSuccId @obj : props, objs)
    where
      (props, objs) = categoryLaws [U nat]