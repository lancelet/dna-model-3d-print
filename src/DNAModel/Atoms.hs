module DNAModel.Atoms where

import qualified DNAModel.CSG.Types            as CSG

import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           Linear.Epsilon                 ( Epsilon )
import           Linear.V3                      ( V3(V3) )
import           Linear.Metric                  ( distance
                                                , normalize
                                                )
import           Linear.Vector                  ( (*^) )

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
data Atom a = Atom Text Element (V3 a)

-- | Geometric atom with origin and radius.
data GeoAtom a = GeoAtom Text (V3 a) a deriving (Eq)

-- | Geometry configuration.
data GeomConfig a
  = GeomConfig
    { gcEpsilon         :: a
    , gcConnectorRadius :: a
    , gcConnectorDepth  :: a
    , gcChamfer         :: a
    }

-- | Convert an Atom to a GeoAtom.
atomToGeoAtom :: (Element -> a) -> Atom a -> GeoAtom a
atomToGeoAtom sizeFn (Atom name el p) = GeoAtom name p (sizeFn el)

-- | Generate the geometry of a protein.
proteinGeom
  :: (Floating a, Epsilon a, Ord a)
  => GeomConfig a    -- ^ Geometry configuration.
  -> (Element -> a)  -- ^ Element atomic radii in Angstrom units.
  -> [Atom a]        -- ^ Atoms.
  -> [CSG.Geom a]    -- ^ Protein geometry.
proteinGeom cfg sizeFn atoms = geos
 where
  geoAtoms      = atomToGeoAtom sizeFn <$> atoms
  neighbourSets = zip geoAtoms (intersectingAtoms geoAtoms <$> geoAtoms)
  geos          = uncurry (atomGeom cfg) <$> neighbourSets

-- | Find the atoms that intersect a given atom.
intersectingAtoms
  :: (Ord a, Floating a)
  => [GeoAtom a]  -- ^ Potential neighbours.
  -> GeoAtom a    -- ^ Main atom.
  -> [GeoAtom a]  -- ^ Actual intersecting neighbours.
intersectingAtoms potentialNeighbours atm = neighbours
 where
  neighbours = filter (isNeighbour atm) potentialNeighbours
  isNeighbour a1@(GeoAtom _ p1 r1) a2@(GeoAtom _ p2 r2)
    | a1 == a2  = False
    | otherwise = distance p1 p2 < r2 + r1

-- | Produce geometry for an atom.
atomGeom
  :: (Floating a, Epsilon a)
  => GeomConfig a  -- ^ Geometry configuration.
  -> GeoAtom a     -- ^ Main atom.
  -> [GeoAtom a]   -- ^ Neighbouring atoms.
  -> CSG.Geom a    -- ^ Produced geometry.
atomGeom cfg atm neighbours = CSG.GeomName
  name
  (CSG.GeomDifference (CSG.sphere r & CSG.translate p) subGeomList)
 where
  GeoAtom name p r = atm
  subGeomList      = subtractionGeomForPair cfg atm <$> neighbours

-- | Subtraction geometry for a pair of atoms.
subtractionGeomForPair
  :: (Floating a, Epsilon a)
  => GeomConfig a  -- ^ Geometry configuration.
  -> GeoAtom a     -- ^ Main atom.
  -> GeoAtom a     -- ^ Neighbouring atom.
  -> CSG.Geom a    -- ^ Produced geometry.
subtractionGeomForPair cfg atm neighbour =
  defaultSubtractionGeom cfg r1 rb
    & CSG.lookAt (-r12) (V3 0 0 1)
    & CSG.translate (p1 + rb *^ r12)
 where
  r12             = normalize (p2 - p1)
  rb              = (d12 * d12 + r1 * r1 - r2 * r2) / (2 * d12)
  d12             = distance p1 p2
  GeoAtom _ p1 r1 = atm
  GeoAtom _ p2 r2 = neighbour

-- | Default subtraction geometry for a connector.
--
-- This geometry is centred at the origin and aligned with the z-axis.
defaultSubtractionGeom
  :: Floating a
  => GeomConfig a  -- ^ Geometry configuration.
  -> a             -- ^ Atom radius.
  -> a             -- ^ Radius at the connector base.
  -> CSG.Geom a    -- ^ Produced geometry.
defaultSubtractionGeom cfg r rb = CSG.GeomUnion [cylinder, cuboid, cone]
 where
  cylinder = CSG.cylinder cr (cd + eps) & CSG.translate (V3 0 0 (-eps))
  cuboid   = CSG.cuboid cuboidW cuboidW (r - rb + eps)
    & CSG.translate (V3 0 0 (-(r - rb + eps) / 2))
  cuboidW                      = eps + 2 * sqrt (r * r - rb * rb)
  cone = CSG.cone coneR coneR & CSG.translate (V3 0 0 (-eps))
  coneR                        = cr + chamfer + eps
  GeomConfig eps cr cd chamfer = cfg
