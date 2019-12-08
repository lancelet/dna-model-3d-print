{-# LANGUAGE OverloadedStrings #-}
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
  = Hydrogen
  | Carbon
  | Nitrogen
  | Oxygen
  | Phosphorus
  deriving Eq

-- | Atom output: real or part of dummy geometry.
data Output
  = Output
  | Dummy
  deriving Eq

-- | Element atomic radii in Angstrom units.
elementRadius :: Fractional a => Element -> a
elementRadius e = case e of
  Hydrogen   -> 0.25
  Carbon     -> 0.70
  Nitrogen   -> 0.65
  Oxygen     -> 0.60
  Phosphorus -> 1.00

-- | Atom with Angstrom position and element name.
data Atom a = Atom Output Text Element (V3 a) deriving Eq

-- | Check if an atom is a dummy.
isDummyAtom :: Atom a -> Bool
isDummyAtom (Atom o _ _ _) = case o of
  Dummy  -> True
  Output -> False

-- | Geometry configuration.
data GeomConfig a
  = GeomConfig
    { gcEpsilon             :: a
    , gcConnectorRadius     :: a
    , gcConnectorDepth      :: a
    , gcChamfer             :: a
    , gcHydrogenSocketDepth :: a
    , gcHydrogenPlugDepth   :: a
    }

-- | Convert an Atom to a GeoAtom.
--atomToGeoAtom :: (Element -> a) -> Atom a -> GeoAtom a
--atomToGeoAtom sizeFn (Atom name el p) = GeoAtom name p (sizeFn el)

-- | Generate the geometry of a protein.
proteinGeom
  :: (Floating a, Epsilon a, Ord a)
  => GeomConfig a    -- ^ Geometry configuration.
  -> (Element -> a)  -- ^ Element atomic radii in Angstrom units.
  -> [Atom a]        -- ^ Atoms.
  -> [CSG.Geom a]    -- ^ Protein geometry.
proteinGeom cfg radiusFn atoms = geos
 where
  neighbourSets =
    filter (\(atm, _neighbours) -> (not . isDummyAtom) atm)
      $   zip atoms
      $   intersectingAtoms radiusFn atoms
      <$> atoms
  geos = uncurry (atomGeom cfg radiusFn) <$> neighbourSets

-- | Find the atoms that intersect a given atom.
intersectingAtoms
  :: (Ord a, Floating a)
  => (Element -> a)  -- ^ Atom radius function.
  -> [Atom a]        -- ^ Potential neighbours.
  -> Atom a          -- ^ Main atom.
  -> [Atom a]        -- ^ Actual intersecting neighbours.
intersectingAtoms radiusFn potentialNeighbours atm = neighbours
 where
  neighbours = filter (isNeighbour atm) potentialNeighbours
  isNeighbour a1@(Atom _ _ e1 p1) a2@(Atom _ _ e2 p2)
    | a1 == a2  = False
    | otherwise = distance p1 p2 < r2 + r1
   where
    r1 = radiusFn e1
    r2 = radiusFn e2

-- | Produce geometry for an atom.
atomGeom
  :: (Floating a, Epsilon a)
  => GeomConfig a    -- ^ Geometry configuration.
  -> (Element -> a)  -- ^ Atom radius function.
  -> Atom a          -- ^ Main atom.
  -> [Atom a]        -- ^ Neighbouring atoms.
  -> CSG.Geom a      -- ^ Produced geometry.
atomGeom cfg radiusFn atm neighbours = CSG.GeomName
  name
  (CSG.GeomDifference (CSG.sphere r & CSG.translate p) subGeomList)
 where
  Atom _ name e p = atm
  r               = radiusFn e
  subGeomList     = subGeom atm <$> neighbours
  subGeom a h@(Atom _ _ Hydrogen _) = hydrogenPlugGeomForPair cfg radiusFn a h
  subGeom a o                       = subtractionGeomForPair cfg radiusFn a o

-- | Subtraction geometry for a pair of atoms.
subtractionGeomForPair
  :: (Floating a, Epsilon a)
  => GeomConfig a    -- ^ Geometry configuration.
  -> (Element -> a)  -- ^ Atom radius function.
  -> Atom a          -- ^ Main atom.
  -> Atom a          -- ^ Neighbouring atom.
  -> CSG.Geom a      -- ^ Produced geometry.
subtractionGeomForPair cfg radiusFn atm neighbour =
  defaultSubtractionGeom cfg r1 rb
    & CSG.lookAt (-r12) (V3 0 0 1)
    & CSG.translate (p1 + rb *^ r12)
 where
  r12            = normalize (p2 - p1)
  rb             = (d12 * d12 + r1 * r1 - r2 * r2) / (2 * d12)
  d12            = distance p1 p2
  Atom _ _ e1 p1 = atm
  Atom _ _ e2 p2 = neighbour
  r1             = radiusFn e1
  r2             = radiusFn e2

-- | Subtraction geometry for a hydrogen atom plug.
hydrogenPlugGeomForPair
  :: (Floating a, Epsilon a)
  => GeomConfig a            -- ^ Geometry configuration.
  -> (Element -> a)          -- ^ Atom radius function.
  -> Atom a                  -- ^ Main atom (should be Carbon)
  -> Atom a                  -- ^ Neighbouring atom (should be Hydrogen)
  -> CSG.Geom a              -- ^ Produced geometry.
hydrogenPlugGeomForPair cfg radiusFn atm neighbour =
  defaultHydrogenPlugGeom cfg (radiusFn Hydrogen)
    & CSG.lookAt (-r12) (V3 0 0 1)
    & CSG.translate (p1 + r1 *^ r12)
 where
  r12            = normalize (p2 - p1)
  Atom _ _ e1 p1 = atm
  Atom _ _ _  p2 = neighbour
  r1             = radiusFn e1

-- | Default subtraction geometry for a connector.
--
-- This geometry is centred at the origin and aligned with the z-axis.
{-
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
  cuboidW                          = eps + 2 * sqrt (r * r - rb * rb)
  cone = CSG.cone coneR coneR & CSG.translate (V3 0 0 (-eps))
  coneR                            = cr + chamfer + eps
  GeomConfig eps cr cd chamfer _ _ = cfg
-}
defaultSubtractionGeom cfg r rb = CSG.GeomUnion [flatCuboid, connectorCuboid]
  where
  connectorCuboid = CSG.cuboid (2*cr) (2*cr) (cd+eps)
    & CSG.translate (V3 0 0 (cd/2))
  flatCuboid = CSG.cuboid flatCuboidW flatCuboidW (r - rb + eps)
    & CSG.translate (V3 0 0 (-(r - rb + eps) / 2))
  flatCuboidW = eps + 2 * sqrt (r * r - rb * rb)
  GeomConfig eps cr cd chamfer _ _ = cfg

-- | Default hydrogen plug subtraction geometry.
--
-- Centred at the origin and aligned with the z-axis.
defaultHydrogenPlugGeom
  :: Floating a
  => GeomConfig a  -- ^ Geometry configuration.
  -> a             -- ^ Hydrogen atom radius.
  -> CSG.Geom a    -- ^ Produced geometry.
defaultHydrogenPlugGeom cfg rb = CSG.GeomDifference
  (CSG.GeomUnion [cone, cylinder])
  [cuboid]
 where
  cone      = CSG.cone coneR coneR & CSG.translate (V3 0 0 (-eps))
  coneR     = 0.9 * rb + eps
  cylinder  = CSG.cylinder cylinderR pd & CSG.translate (V3 0 0 sd)
  cylinderR = 0.9 * rb - sd
  cuboid    = CSG.cuboid coneR coneR cuboidH
    & CSG.translate (V3 0 0 (sd + pd + cuboidH / 2))
  cuboidH                    = (coneR - pd) / 2
  GeomConfig eps _ _ _ sd pd = cfg
