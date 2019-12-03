module DNAModel.CSG.Types
  ( Scene(..)
  , Geom(..)
  , Prim(..)
  , Radius(..)
  , Width(..)
  , Depth(..)
  , Height(..)
  , XForm(..)
  , Name(..)
  , Sphere(..)
  , Cylinder(..)
  , Cone(..)
  , Cuboid(..)
  , sphere
  , cylinder
  , cone
  , cuboid
  , translate
  )
where

import           Data.Text                      ( Text )
import           Linear.V3                      ( V3(V3) )
import           Linear.V4                      ( V4(V4) )
import           Linear.Matrix                  ( M44 )

newtype Scene a = Scene { unScene :: [Geom a] } deriving Show

data Geom a
  = GeomPrim (Prim a)
  | GeomTransform (XForm a) (Geom a)
  | GeomUnion [Geom a]
  | GeomDifference (Geom a) [Geom a]
  deriving Show

data Prim a
  = PrimSphere (Sphere a)
  | PrimCylinder (Cylinder a)
  | PrimCone (Cone a)
  | PrimCuboid (Cuboid a)
  deriving Show

newtype Radius a = Radius a deriving Show
newtype Width a = Width a deriving Show
newtype Depth a = Depth a deriving Show
newtype Height a = Height a deriving Show
newtype XForm a = XForm (M44 a) deriving Show
newtype Name = Name Text deriving Show
newtype Sphere a = Sphere (Radius a) deriving Show
data Cylinder a = Cylinder (Radius a) (Height a) deriving Show
data Cone a = Cone (Radius a) (Height a) deriving Show
data Cuboid a = Cuboid (Width a) (Depth a) (Height a) deriving Show

translation :: Num a => V3 a -> XForm a
translation (V3 x y z) = XForm m44
 where
  m44 = V4 r1 r2 r3 r4
  r1  = V4 1 0 0 x
  r2  = V4 0 1 0 y
  r3  = V4 0 0 1 z
  r4  = V4 0 0 0 1

applyXForm :: XForm a -> Geom a -> Geom a
applyXForm = GeomTransform

translate :: Num a => V3 a -> Geom a -> Geom a
translate v = applyXForm (translation v)

sphere
  :: a       -- ^ Radius.
  -> Geom a  -- ^ Sphere geometry.
sphere r = GeomPrim . PrimSphere . Sphere . Radius $ r

cylinder
  :: a       -- ^ Radius.
  -> a       -- ^ Height.
  -> Geom a  -- ^ Cylinder geometry.
cylinder r h = GeomPrim . PrimCylinder $ Cylinder (Radius r) (Height h)

cone
  :: a       -- ^ Radius.
  -> a       -- ^ Height.
  -> Geom a  -- ^ Cone geometry.
cone r h = GeomPrim . PrimCone $ Cone (Radius r) (Height h)

cuboid
  :: a       -- ^ Width (x).
  -> a       -- ^ Depth (y).
  -> a       -- ^ Height (z).
  -> Geom a  -- ^ Cuboid geometry.
cuboid x y z = GeomPrim . PrimCuboid $ Cuboid (Width x) (Depth y) (Height z)
