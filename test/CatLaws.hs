module CatLaws where

import CategAlgebra
import Data.Typeable (Typeable, cast, showsTypeRep, typeOf)
import Data.Maybe (fromJust)
import Hedgehog (Gen, PropertyT, forAll)
import Hedgehog.Function.Internal
    ( fnWith, forAllFn, Arg, CoGen, Fn, Vary(vary) )

-- ! ---------------------------------------------------------------------- ! --
-- ! Instances ! --
data (Categorical a) => NamedSet a = 
  NamedSet {setName :: String, gen :: Gen a, cogen :: CoGen a}
data MorphismImpl = forall a b. (Show b, Eq b, Typeable b, Show a, Eq a, Typeable a) => 
  Morphism {f :: a -> b} deriving (Typeable)
data HomImpl = forall a b. (Show a, Eq a, Typeable a, Show b, Eq b, Typeable b) =>
  Hom {dom :: Gen a, morphs :: Gen (Fn a b)}

instance Show MorphismImpl where
  show (Morphism f) = (showsTypeRep $ typeOf f) ""

-- type instance forall s. UFormula s = [UU] -> UU
-- type instance forall s. Prop s = PropertyT IO Bool
-- type instance forall s. PFormula s = [UU] -> PropertyT IO Bool
  
instance Universum HomImpl where
  type UFormula HomImpl = [UU] -> UU
  quote m _ = m
  pick n env = env !! n
  
instance Determinable HomImpl where
  type Prop HomImpl = PropertyT IO Bool
  contain (Hom (dom :: Gen a) (ms :: Gen (Fn a b))) (UU v) = not <$> do
    d <- forAll dom
    f <- forAllFn ms
    case cast v of
      Just (Morphism g) -> pure $
        case cast d of
          Just x -> (fromJust . cast $ f d) /= g x -- x ~ d
          _ -> True
      _ -> pure True

  eql (Hom (dom :: Gen a) _) (UU v1) (UU v2) = do
    d <- forAll dom
    case cast v1 of
      Just (Morphism f) -> case cast v2 of
        Just (Morphism g) ->
          let x = fromJust $ cast d
            in pure $ f (fromJust $ cast d) == (fromJust . cast $ g x)
        _ -> pure False
      _ -> pure False

instance LogicAlgebra HomImpl where
  type PFormula HomImpl = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF h x y e = eql h (x e) (y e)
  forall' (Hom _ (morphs :: Gen (Fn a b))) j e = do
    m <- forAllFn morphs
    j $ UU (Morphism m) : e

instance Universum (NamedSet a) where
  type UFormula (NamedSet a) = [UU] -> UU
  quote m _ = m
  pick n env = env !! n

instance (Show a, Eq a, Typeable a, Arg a) => Determinable (NamedSet a) where
  type Prop (NamedSet a) = PropertyT IO Bool
  contain (NamedSet _ ga _) (UU v) = not <$> do
    el <- forAll ga
    let x = fromJust $ cast v
      in pure $ x /= el

  eql (NamedSet _ (_ :: Gen a) _) (UU (v1 :: t)) (UU v2) = do
    let x = fromJust $ cast @t @a v1
        y = fromJust $ cast v2
      in pure $ y == x

instance (Show a, Eq a, Typeable a, Arg a) => LogicAlgebra (NamedSet a) where
  type PFormula (NamedSet a) = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF h x y e = eql h (x e) (y e)
  forall' (NamedSet _ ga _) j e = do
    a <- forAll ga
    j $ UU a : e
 
instance CategAlgebra NamedSet where
  type Morphism NamedSet = MorphismImpl
  type Hom NamedSet = HomImpl

  hom (NamedSet _ ga cga) (NamedSet _ gb _) = Hom ga $ fnWith cga gb
  comp (Morphism f) (Morphism g) = Morphism (g . fromJust . cast . f)
  compF x y e = go (x e) (y e)
    where
      go (UU f') (UU g') =
        let f = fromJust . cast $ f'
            g = fromJust . cast $ g'
          in UU $ comp @NamedSet f g

  idm (NamedSet _ (_ :: Gen a) _) = Morphism $ id @a
  unitO = NamedSet "Unit" (pure ()) vary