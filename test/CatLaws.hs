module CatLaws where

import CategAlgebra
import Data.Typeable
import Data.Maybe (fromJust)
import Hedgehog
import Hedgehog.Function.Internal

-- ! ---------------------------------------------------------------------- ! --
-- ! Instances ! --
data (Show a, Eq a, Typeable a, Arg a) => NamedSet a = 
  NamedSet {setName :: String, gen :: Gen a, cogen :: CoGen a, startVal :: a}
data MorphismImpl = forall a b. (Show b, Eq b, Typeable b, Show a, Eq a, Typeable a) => 
  Morphism {val :: a, f :: a -> b} deriving (Typeable)
data HomImpl = forall a b. (Show a, Eq a, Typeable a, Show b, Eq b, Typeable b) =>
  Hom {dom :: Gen a, morphs :: Gen (Fn a b)}

instance Eq MorphismImpl where
  (Morphism v1 f) == (Morphism v2 g) =
    --v1 == v2 && f v1 == g v1
    case cast v2 of
      Just x -> x == v1 &&
        case cast $ g v2 of
          Just y -> f v1 == y
          _ -> False
      _ -> False

instance Show MorphismImpl where
  show (Morphism v f) = (showsTypeRep $ typeOf v) " -> " ++ showsTypeRep (typeOf $ f v) ""

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
      Just (Morphism _ g) -> pure $
        case cast d of
          Just x -> Morphism d f /= Morphism x g -- x ~ d
          _ -> True
      _ -> pure True

  eql (Hom (dom :: Gen a) _) (UU v1) (UU v2) = do
    d <- forAll dom
    case cast v1 of
      Just (Morphism _ f) -> case cast v2 of
        Just (Morphism _ g) -> pure $ Morphism (fromJust $ cast d) f == Morphism (fromJust $ cast d) g
        _ -> pure True
      _ -> pure True

instance LogicAlgebra HomImpl where
  type PFormula HomImpl = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF h x y e = eql h (x e) (y e)
  forall' (Hom (dom :: Gen a) (morphs :: Gen (Fn a b))) j e = do
    v <- forAll dom
    m <- forAllFn morphs
    j $ UU (Morphism v m) : e

instance Universum (NamedSet a) where
  type UFormula (NamedSet a) = [UU] -> UU
  quote m _ = m
  pick n env = env !! n

instance (Show a, Eq a, Typeable a, Arg a) => Determinable (NamedSet a) where
  type Prop (NamedSet a) = PropertyT IO Bool
  contain (NamedSet _ ga _ _) (UU v) = not <$> do
    el <- forAll ga
    let x = fromJust $ cast v
      in pure $ x /= el

  eql (NamedSet _ _ _ st) (UU v1) (UU v2) = not <$> do
    let x = fromJust $ cast v1
        y = fromJust $ cast v2
        _ = st == x -- to specify the type to which x and y are casted
      in pure $ y /= x

instance (Show a, Eq a, Typeable a, Arg a) => LogicAlgebra (NamedSet a) where
  type PFormula (NamedSet a) = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF h x y e = eql h (x e) (y e)
  forall' (NamedSet _ ga _ _) j e = do
    a <- forAll ga
    j $ UU a : e
 
instance CategAlgebra NamedSet where
  type Morphism NamedSet = MorphismImpl
  type Hom NamedSet = HomImpl

  hom (NamedSet _ ga cga _) (NamedSet _ gb _ _) = Hom ga $ fnWith cga gb
  comp (Morphism v f) (Morphism _ g) = Morphism v (g . fromJust . cast . f)
  compF x y e = go (x e) (y e)
    where
      go (UU f') (UU g') =
        let f = fromJust . cast $ f'
            g = fromJust . cast $ g'
          in UU $ comp @NamedSet f g

  idm (NamedSet _ _ _ sv) = Morphism sv id
  unitO = NamedSet "Unit" (pure ()) vary ()