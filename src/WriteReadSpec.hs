module WriteReadSpec where

import CategAlgebra
import Data.Typeable (Typeable, cast, showsTypeRep, typeOf)
import Data.Int(Int8, Int32 )
import Data.Word (Word64)
import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import Data.Bits ((.|.))
import Data.Functor.Contravariant ((>$<))
import CategNatAlgebra (Nat(..))
import Foreign.C.Types
import Foreign.Ptr(Ptr, castPtr)
import Foreign.C (CString, castCCharToChar, newCString, peekCString, castCharToCChar)
import Hedgehog (Gen, PropertyT)
import Hedgehog.Internal.Property (forAll, evalIO)
import Hedgehog.Internal.Gen (string, alpha, integral)
import Hedgehog.Internal.Range (constant)
import Hedgehog.Function.Internal (fnWith, forAllFn, Arg, CoGen, Fn, Vary(vary))
import GHC.Generics (Generic)
import Data.Kind (Type, Constraint)

{- |
  In C type signature is:

  int open(const char *path, int flags, mode_t mode);

  * mode_t ~ unsigned short

  For specification following options are needed:

  * flags: O_RDWR | O_CREAT
  * mode: S_IRUSR | S_IWUSR
-}
foreign import capi "fcntl.h open" open :: CString -> CInt -> CUShort -> IO CInt
{- |
  In C type signature is:

  int close(int fd);

  For specification following options are needed:

  * fd >= 0
-}
foreign import capi "unistd.h close" close :: CInt -> IO CInt
{- |
  In C type signature is:

  ssize_t write (int fd, void* buf, size_t count); 

  * size_t ~ unsigned long
  * ssize_t ~ long

  For specification following options are needed:

    * fd >= 0
    * count <= len(buf)
-}
foreign import capi "unistd.h write" write :: CInt -> Ptr () -> CULong -> IO CLong
{- |
  In C type signature is:

  * ssize_t read (int fd, void* buf, size_t count);
  * size_t ~ unsigned long

  ssize_t ~ long

  For specification following options are needed:

    * fd >= 0
    * count <= len(buf)
-}
foreign import capi "unistd.h read" read :: CInt -> Ptr () -> CULong -> IO CLong

type family Categorical (t :: Type) :: Constraint
type instance Categorical t = (Typeable t, Eq t, Show t, Arg t)
newtype Path = Path {p :: [CChar]} deriving (Typeable, Eq, Show, Generic)
deriving instance Generic CChar
instance Arg CChar
instance Arg Path
newtype Buf = PreCString {str :: [CChar]} deriving (Typeable, Eq, Show, Generic)
instance Arg Buf


class CategAlgebra ioObj => WriteReadSpec ioObj where
  path :: (LogicAlgebra (ioObj Path), Categorical Path) =>
    ioObj Path
  
  count :: (LogicAlgebra (ioObj Nat), Categorical Nat) =>
    ioObj Nat

  buf :: (LogicAlgebra (ioObj Buf), Categorical Buf) =>
    ioObj Buf

  writeRead :: UU -> UU -> Morphism ioObj
  writeReadF :: UFormula (Hom ioObj) -> UFormula (Hom ioObj) -> UFormula (Hom ioObj)

  writeReadId :: (LogicAlgebra (ioObj Nat), Categorical Nat,
    LogicAlgebra (ioObj Path), Categorical Path,
    LogicAlgebra (ioObj Buf), Categorical Buf) => (String, Prop (Hom ioObj))
  writeReadId = ("WriteReadId", convertTo @ioObj $ ground @(ioObj Nat) sp [])
    where
      bufHom = hom @ioObj buf buf
      quote' = quote @(Hom ioObj)
      pick' = pick @(Hom ioObj)
      sp =
        forall' @(ioObj Nat) count . pathToNat @ioObj .
        forall' @(ioObj Path) path . convertFrom @ioObj .
        eqlF bufHom (quote' . UU $ idm @ioObj buf) $
        writeReadF @ioObj (pick' 0) (pick' 1)

  pathToNat :: PFormula (ioObj Path) -> PFormula (ioObj Nat)
  convertTo :: Prop (ioObj Nat) -> Prop (Hom ioObj)
  convertFrom :: PFormula (Hom ioObj) -> PFormula (ioObj Path)

  writeReadSpec :: (LogicAlgebra (ioObj ()), LogicAlgebra (ioObj Path), LogicAlgebra (ioObj Nat),
    LogicAlgebra (ioObj Buf)) 
    => ([(String, Prop (Hom ioObj))], [EvalUnit ioObj])
  writeReadSpec = (writeReadId @ioObj : props, objs)
    where
      (props, objs) = categoryLaws [U path, U count, U buf]

-- ! ----------Instances and interpreters---------------- ! --
data (Categorical a) => NamedIOSet a =
  NamedIOSet {ioSetName :: String, ioGen :: Gen a, ioCoGen :: CoGen a}
data IOMorphism = forall a b. (Show b, Eq b, Typeable b, Show a, Eq a, Typeable a) =>
  IOMorphism {f :: IO a -> IO b} deriving (Typeable)
data IOHom = forall a b. (Show a, Eq a, Typeable a, Show b, Eq b, Typeable b) =>
  IOHom {ioDom :: Gen (IO a), ioMorphs :: Gen (Fn a (IO b))}
  

instance Show (IO a) where
  show _ = "IO object"

instance Universum IOHom where
  type UFormula IOHom = [UU] -> UU
  quote m _ = m
  pick n env = env !! n

instance Determinable IOHom where
  type Prop IOHom = PropertyT IO Bool
  contain (IOHom (dom :: Gen (IO a)) (ms :: Gen (Fn a (IO b)))) (UU v) = not <$> do
    d <- forAll dom
    f <- forAllFn ms
    case cast v of
      Just (IOMorphism g) ->
        let x = fromJust $ cast d -- x ~ d
            y = fromJust . cast <$> g x
          in evalIO $ (/=) <$> (d >>= f) <*> y
      _ -> pure True

  eql (IOHom (dom :: Gen (IO a)) _) (UU v1) (UU v2) = do
    d <- forAll dom
    case cast v1 of
      Just (IOMorphism f) ->
        case cast v2 of
          Just (IOMorphism g) ->
            let x = f . fromJust $ cast d
                y = g . fromJust $ cast d
              in evalIO $ (==) <$> x <*> fromJust (cast y)
          _ -> pure False
      _ -> pure False

instance LogicAlgebra IOHom where
  type PFormula IOHom = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF h x y e = eql h (x e) (y e)
  forall' (IOHom _ (morphs :: Gen (Fn a (IO b)))) j e = do
    m <- forAllFn morphs
    j $ UU (IOMorphism (m =<<)) : e

instance Show IOMorphism where
  show (IOMorphism f) = (showsTypeRep $ typeOf f) ""

instance Universum (NamedIOSet a) where
  type UFormula (NamedIOSet a) = [UU] -> UU
  quote m _ = m
  pick n env = env !! n

instance (Show a, Eq a, Typeable a, Arg a) => Determinable (NamedIOSet a) where
  type Prop (NamedIOSet a) = PropertyT IO Bool
  contain (NamedIOSet _ g _) (UU v) = not <$> do
    el <- forAll g
    let x = fromJust $ cast v
      in evalIO $ (/=) <$> x <*> pure el

  eql (_ :: NamedIOSet a) (UU v1) (UU (v2 :: t)) = do
    let x = fromJust $ cast v1
        y = fromJust $ cast @t @(IO a) v2
      in evalIO $ (==) <$> x <*> y

instance (Show a, Eq a, Typeable a, Arg a) => LogicAlgebra (NamedIOSet a) where
  type PFormula (NamedIOSet a) = [UU] -> PropertyT IO Bool
  ground p = p
  elemF h x e = contain h $ x e
  eqlF s x y e = eql s (x e) (y e)
  forall' (NamedIOSet _ ga _) j e = do
    a <- forAll ga
    j $ UU (pure @IO a) : e

instance CategAlgebra NamedIOSet where
  type Morphism NamedIOSet = IOMorphism
  type Hom NamedIOSet = IOHom
  hom (NamedIOSet _ ga cga) (NamedIOSet _ gb _) = IOHom (pure <$> ga) $ fnWith cga (pure <$> gb)
  comp (IOMorphism f) (IOMorphism g) = IOMorphism $ g . fromJust . cast . f
  compF x y e = go (x e) (y e)
    where
      go (UU f') (UU g') =
        let f = fromJust . cast $ f'
            g = fromJust . cast $ g'
          in UU $ comp @NamedIOSet f g

  idm (_ :: NamedIOSet a) = IOMorphism $ id @(IO a)
  unitO = NamedIOSet "Unit" (pure ()) vary


instance WriteReadSpec NamedIOSet where
  path = NamedIOSet "Path" pathGen pathCoGen
  count = NamedIOSet "Count type" (N <$> integral (constant 0 10)) vary
  buf = NamedIOSet "Buffer" bufGen bufCoGen
  writeRead (UU (path' :: t1)) (UU (count' :: t2)) =
    IOMorphism $ \(buf' :: IO Buf) -> do
      (PreCString prebuf) <- buf'
      let buf'' = map castCCharToChar prebuf
          prepath = fromJust $ cast @t1 @(IO Path) path'
          path'' = prepath >>= \(Path p) -> pure $ map castCCharToChar p
          count'' = (.n) <$> fromJust (cast @t2 @(IO Nat) count')
      path <- path''
      count <- count'' >>= \c -> pure $ min c (fromIntegral $ length buf'' - 1)
      buf <- newCString buf''
      cpath <- newCString path
      fd@(CInt n) <- open cpath (CInt $ 0x0002 .|. 0x00000200) (CUShort $ 0000400 .|. 0000200) -- TODO Think about constants
      if n < 0 then fromJust Nothing
        else do
          wnbs <- write fd (castPtr buf) (CULong $ intToWord count)
          _ <- close fd
          fd1@(CInt n1) <- open cpath (CInt 0x0002) (CUShort 0)
          if n1 < 0 then fromJust Nothing else do
              _ <- WriteReadSpec.read fd1 (castPtr buf) (longToULong wnbs)
              _ <- close fd1
              str <- peekCString buf
              pure . PreCString $ map castCharToCChar str

  writeReadF x y e = UU $ writeRead @NamedIOSet (x e) (y e)
  convertTo = id
  convertFrom = id
  pathToNat = id


intToWord :: Int32 -> Word64
intToWord = fromIntegral . abs -- TODO This should be implemented differently

longToULong :: CLong -> CULong
longToULong (CLong n) = CULong . fromIntegral $ abs n -- TODO This should be implemented differently

pathGen :: Gen Path
pathGen = Path . (\s -> map castCharToCChar $ "/Users/nikita/hedge/tmp/" ++ s ++ ".txt")
  <$> string (constant 5 15) alpha

pathCoGen :: CoGen Path
pathCoGen = go >$< (vary :: (CoGen [Int8]))
  where
    go (Path p') =
      let prep = replaceWithAlpha $ map castCCharToChar p'
          p = "/Users/nikita/hedge/tmp/" ++ prep ++ ".txt"
        in map ((\(CChar n) -> n) . castCharToCChar) p

replaceWithAlpha :: String -> String
replaceWithAlpha [] = []
replaceWithAlpha (c:cs) = if isAlpha c then c : replaceWithAlpha cs else 'a': replaceWithAlpha cs

bufGen :: Gen Buf
bufGen = PreCString . map castCharToCChar <$> string (constant 10 20) alpha

bufCoGen :: CoGen Buf
bufCoGen = go >$< (vary :: (CoGen [Int8]))
  where
    go (PreCString cs) = 
      let pres = replaceWithAlpha $ map castCCharToChar cs
        in map ((\(CChar n) -> n) . castCharToCChar) pres
