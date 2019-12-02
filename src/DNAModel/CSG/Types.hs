module DNAModel.CSG.Types where

import Data.Text (Text)
import Linear.Matrix (M44)

newtype Scene a = Scene [Object a]

data Object a = Object (Prop a) (Geom a) deriving Show

data Prop a = Prop (Maybe Name) (Maybe (Color a)) deriving Show

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
newtype Height a = Height a deriving Show
newtype Depth a = Depth a deriving Show
newtype XForm a = XForm (M44 a) deriving Show
newtype Name = Name Text deriving Show
newtype Sphere a = Sphere (Radius a) deriving Show
data Cylinder a = Cylinder (Radius a) (Height a) deriving Show
data Cone a = Cone (Radius a) (Height a) deriving Show
data Cuboid a = Cuboid (Width a) (Depth a) (Height a) deriving Show
data Color a = Color a a a deriving Show

