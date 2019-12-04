module DNAModel.Atoms where

import qualified DNAModel.CSG.Types as CSG
import qualified DNAModel.CSG.RhinoExport as R

import Data.Function ((&))
import qualified Data.Text as Text
import Linear.Epsilon (Epsilon)
import Linear.V3 (V3(V3))
import Linear.Metric (distance, normalize)
import Linear.Vector ((*^))

import Debug.Trace (trace)

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
    , gcChamfer         :: a
    }

-- | Produce geometry for an atom.
atomGeom
  :: GeomConfig a  -- ^ Geometry configuration.
  -> GeoAtom a     -- ^ Main atom.
  -> [GeoAtom a]   -- ^ Neighbouring atoms.
  -> CSG.Geom a    -- ^ Produced geometry.
atomGeom cfg atm neighbours = undefined

{-
test :: IO ()
test = do
  let cfg = GeomConfig (1 :: Double) 4 5 0.5
  let geom = subtractionGeom cfg 10 8
  let aGeom = CSG.sphere 10 & CSG.translate (V3 0 0 8)
  putStrLn $ Text.unpack $ R.scene (CSG.Scene [aGeom, geom])
-}
test :: IO ()
test = do
  let cfg = GeomConfig (1 :: Double) 4 5 0.5
  let atm = GeoAtom (V3 0 0 0) 10
  let neighbour = GeoAtom (V3 15 0 0) 8
  let scene = CSG.Scene
        [ CSG.sphere 10
        , CSG.sphere 8 & CSG.translate (V3 15 0 0)
        , subtractionGeomForPair cfg atm neighbour 
        ]
  putStrLn $ Text.unpack $ R.scene scene

-- | Subtraction geometry for a pair of atoms.
subtractionGeomForPair
  :: (Floating a, Epsilon a, Show a)
  => GeomConfig a  -- ^ Geometry configuration.
  -> GeoAtom a     -- ^ Main atom.
  -> GeoAtom a     -- ^ Neighbouring atom.
  -> CSG.Geom a    -- ^ Produced geometry.
subtractionGeomForPair cfg atm neighbour
  = defaultSubtractionGeom cfg r1 (trace (show rb) rb)
  & CSG.lookAt (-r12) (V3 0 0 1)
  & CSG.translate (p1 + rb *^ r12)
  where
    r12 = normalize (p2 - p1)
    rb = (d12*d12 + r1*r1 - r2*r2) / (2 * d12)
    d12 = distance p1 p2
    GeoAtom p1 r1 = atm
    GeoAtom p2 r2 = neighbour

-- | Default subtraction geometry for a connector.
--
-- This geometry is centred at the origin and aligned with the z-axis.
defaultSubtractionGeom
  :: Floating a
  => GeomConfig a  -- ^ Geometry configuration.
  -> a             -- ^ Atom radius.
  -> a             -- ^ Radius at the connector base.
  -> CSG.Geom a    -- ^ Produced geometry.
defaultSubtractionGeom cfg r rb
  = CSG.GeomUnion
    [ cylinder
    , cuboid
    , cone
    ]
  where
    cylinder = CSG.cylinder cr (cd+eps) & CSG.translate (V3 0 0 (-eps))
    cuboid = CSG.cuboid cuboidW cuboidW (r-rb+eps)
             & CSG.translate (V3 0 0 (-(r-rb+eps)/2))
    cuboidW = eps + 2 * sqrt (r*r - rb*rb)
    cone = CSG.cone coneR coneR & CSG.translate (V3 0 0 (-eps))
    coneR = cr + chamfer + eps
    GeomConfig eps cr cd chamfer = cfg
