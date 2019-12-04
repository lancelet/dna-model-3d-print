{-# LANGUAGE OverloadedStrings #-}
module DNAModel.CSG.RhinoExport where

import qualified DNAModel.CSG.Types            as T

import           Control.Lens                   ( (^.) )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Colour.SRGB.Linear        ( RGB(RGB) )
import           Linear                         ( _x
                                                , _y
                                                , _z
                                                , _w
                                                , V3(V3)
                                                )

testScene :: T.Scene Double
testScene = T.Scene
  [ T.cylinder 0.5 4
  , T.cylinder 0.5 4 & T.lookAt (V3 1 1 1) (V3 0 0 1)
  , T.cylinder 0.5 4 & T.lookAt (V3 1 0 0) (V3 0 0 1)
  , T.cylinder 0.5 4 & T.lookAt (V3 0 1 0) (V3 0 0 1)
  ]

tshow :: Show a => a -> Text
tshow = Text.pack . show

newtype Location = Location [Int]
newtype Id = Id Text

instance Show Id where
  show (Id i) = Text.unpack i

emptyLocation :: Location
emptyLocation = Location []

locationToId :: Location -> Id
locationToId (Location ixs) =
  Id $ "obj_" <> (Text.intercalate "_" $ tshow <$> reverse ixs)

locationMainToId :: Location -> Id
locationMainToId loc = Id (baseId <> "_main")
  where (Id baseId) = locationToId loc

locationAppend :: Location -> Int -> Location
locationAppend (Location ixs) ix = Location (ix : ixs)

listLocations :: Location -> [b] -> [(Location, b)]
listLocations l items =
  fmap (\(ix, x) -> (locationAppend l ix, x)) $ zip [1 ..] items

scene :: (RealFrac a, Show a) => T.Scene a -> Text
scene (T.Scene objects) =
  "import rhinoscriptsyntax as rs\n"
    <> (mconcat $ fmap snd $ fmap (\(loc, obj) -> geom loc obj) $ listLocations
         emptyLocation
         objects
       )

rgbToText :: RealFrac a => RGB a -> Text
rgbToText (RGB r g b) =
  "(" <> conv r <> ", " <> conv g <> ", " <> conv b <> ")"
  where conv x = tshow $ (floor (x * 255.0) :: Int)

geom :: (Fractional a, Show a) => Location -> T.Geom a -> (Id, Text)
geom loc geo = case geo of
  T.GeomPrim p        -> prim loc p
  T.GeomTransform x g -> (locationToId loc, child <> xform)
   where
    (childId, child) = geom loc g
    xform =
      "rs.TransformObject(" <> tshow childId <> ", " <> xformToText x <> ")\n"
  T.GeomUnion gs -> (i, childObjs <> union <> "\n")
   where
    children  = (\(loc', geo') -> geom loc' geo') <$> listLocations loc gs
    childObjs = Text.concat (fmap snd children)
    union     = tshow i <> " = rs.BooleanUnion([" <> items <> "])[0]"
    items     = Text.intercalate "," (tshow . fst <$> children)
    i         = locationToId loc
  T.GeomDifference g gs -> (i, childObjs <> "\n" <> diff)
   where
    diff =
      tshow i
        <> " = rs.BooleanDifference("
        <> tshow (fst subtractee)
        <> ", ["
        <> items
        <> "])[0]"
    items = Text.intercalate "," (tshow . fst <$> subtractors)
    childObjs =
      snd subtractee <> "\n" <> Text.intercalate "\n" (fmap snd subtractors)
    locSubtractee  = locationAppend loc 1
    locSubtractors = locationAppend loc 2
    subtractee     = geom locSubtractee g
    subtractors =
      (\(loc', geo') -> geom loc' geo') <$> listLocations locSubtractors gs
    i = locationToId loc

xformToText :: Show a => T.XForm a -> Text
xformToText (T.XForm m44) =
  "["
    <> rowToList (m44 ^. _x)
    <> ","
    <> rowToList (m44 ^. _y)
    <> ","
    <> rowToList (m44 ^. _z)
    <> ","
    <> rowToList (m44 ^. _w)
    <> "]"
 where
  rowToList r =
    "["
      <> tshow (r ^. _x)
      <> ","
      <> tshow (r ^. _y)
      <> ","
      <> tshow (r ^. _z)
      <> ","
      <> tshow (r ^. _w)
      <> "]"

prim :: (Fractional a, Show a) => Location -> T.Prim a -> (Id, Text)
prim loc p = case p of
  T.PrimSphere   s -> sphere loc s
  T.PrimCylinder c -> cylinder loc c
  T.PrimCone     c -> cone loc c
  T.PrimCuboid   c -> cuboid loc c

sphere :: Show a => Location -> T.Sphere a -> (Id, Text)
sphere loc (T.Sphere (T.Radius radius)) =
  (i, tshow i <> " = rs.AddSphere((0,0,0), " <> tshow radius <> ")\n")
  where i = locationToId loc

cylinder :: Show a => Location -> T.Cylinder a -> (Id, Text)
cylinder loc (T.Cylinder (T.Radius radius) (T.Height height)) =
  ( i
  , tshow i
    <> " = rs.AddCylinder((0,0,0), "
    <> tshow height
    <> ", "
    <> tshow radius
    <> ")\n"
  )
  where i = locationToId loc

cone :: (Num a, Show a) => Location -> T.Cone a -> (Id, Text)
cone loc (T.Cone (T.Radius radius) (T.Height height)) =
  ( i
  , tshow i
    <> " = rs.AddCone((0, 0, "
    <> tshow height
    <> "), "
    <> tshow (-height)
    <> ", "
    <> tshow radius
    <> ")\n"
  )
  where i = locationToId loc

cuboid :: (Fractional a, Show a) => Location -> T.Cuboid a -> (Id, Text)
cuboid loc (T.Cuboid (T.Width w) (T.Depth d) (T.Height h)) =
  ( i
  , tshow i
    <> " = rs.AddBox(["
    <> pt x y (-z)
    <> ", "
    <> pt (-x) y (-z)
    <> ", "
    <> pt (-x) (-y) (-z)
    <> ", "
    <> pt x (-y) (-z)
    <> ", "
    <> pt x y z
    <> ", "
    <> pt (-x) y z
    <> ", "
    <> pt (-x) (-y) z
    <> ", "
    <> pt x (-y) z
    <> "])\n"
  )
 where
  pt p q r = "(" <> tshow p <> ", " <> tshow q <> ", " <> tshow r <> ")"
  i = locationToId loc
  x = w / 2
  y = d / 2
  z = h / 2


