module DNAModel.Atoms where

import qualified DNAModel.CSG.Types as CSG
import qualified DNAModel.CSG.RhinoExport as R

import Data.Function ((&))
import qualified Data.Text as Text
import Linear.V3 (V3(V3))

-- | Element for an atom.
data Element
  = Carbon
  | Nitrogen
  | Oxygen
  | Phosphorus

-- | Element atomic radii in Angstrom units.
elementRadius :: Fractional a => Element -> a
elementRadius e = case e of
  Carbon     -> 0.70
  Nitrogen   -> 0.65
  Oxygen     -> 0.60
  Phosphorus -> 1.00

-- | Atom with Angstrom position and element name.
data Atom a = Atom Element (V3 a)

-- | Geometric atom with origin and radius.
data GeoAtom a = GeoAtom (V3 a) a

data GeomConfig a
  = GeomConfig
    { gcEpsilon         :: a
    , gcConnectorRadius :: a
    , gcConnectorDepth  :: a 
    }

-- | Produce geometry for an atom.
atomGeom
  :: GeomConfig a
  -> Atom a
  -> [Atom a]
  -> CSG.Geom a
atomGeom = undefined

test :: IO ()
test = do
  let cfg = GeomConfig (1 :: Double) 4 5
  let geom = subtractionGeom cfg 10 8
  let aGeom = CSG.sphere 10 & CSG.translate (V3 0 0 8)
  putStrLn $ Text.unpack $ R.scene (CSG.Scene [aGeom, geom])

-- | Default subtraction geometry for a connector.
--
-- TODO: Add cone chamfer.
subtractionGeom
  :: Floating a
  => GeomConfig a  -- ^ Geometry configuration.
  -> a             -- ^ Atom radius.
  -> a             -- ^ Radius at the connector base.
  -> CSG.Geom a    -- ^ Produced geometry.
subtractionGeom cfg r rb
  = CSG.GeomUnion
    [ cylinder
    , cuboid
    ]
  where
    cylinder = CSG.cylinder cr (cd+eps) & CSG.translate (V3 0 0 (-eps))
    cuboid = CSG.cuboid cuboidW cuboidW (r-rb+eps)
             & CSG.translate (V3 0 0 (-(r-rb+eps)/2))
    cuboidW = eps + 2 * sqrt (r*r - rb*rb)
    GeomConfig eps cr cd = cfg
