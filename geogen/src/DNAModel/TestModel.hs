{-# LANGUAGE NegativeLiterals #-}
module DNAModel.TestModel where

import qualified DNAModel.CSG.Types            as CSG
import qualified DNAModel.Bases                as Bases
import qualified DNAModel.Atoms                as Atoms

import           Data.Function                  ( (&) )
import           Linear.Epsilon                 ( Epsilon )
import           Linear.V3                      ( V3(V3) )

config :: Fractional a => Atoms.GeomConfig a
config = Atoms.GeomConfig { Atoms.gcEpsilon             = 0.2
                          , Atoms.gcConnectorRadius     = 0.285
                          , Atoms.gcConnectorDepth      = 0.5
                          , Atoms.gcChamfer             = 0.05
                          , Atoms.gcHydrogenSocketDepth = 0.2
                          , Atoms.gcHydrogenPlugDepth   = 0.1
                          }

sizeFn :: Fractional a => Atoms.Element -> a
sizeFn e = Atoms.elementRadius e * (1.0 / 0.7)

guanine :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
guanine =
  CSG.Scene (Atoms.proteinGeom config sizeFn Bases.guanine)
    & CSG.rotateAxisAngleScene (V3 0 -1 0) (radians 5.7)

cytosine :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
cytosine =
  CSG.Scene (Atoms.proteinGeom config sizeFn Bases.cytosine)
    & CSG.rotateAxisAngleScene (V3 0 -1 0) (radians 5.7)

adenine :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
adenine =
  CSG.Scene (Atoms.proteinGeom config sizeFn Bases.adenine)
    & CSG.rotateAxisAngleScene (V3 0 -1 0) (radians 5.7)

thymine :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
thymine =
  CSG.Scene (Atoms.proteinGeom config sizeFn Bases.thymine)
    & CSG.rotateAxisAngleScene (V3 0 -1 0) (radians 5.7)

spineUnit :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
spineUnit = CSG.Scene (Atoms.proteinGeom config sizeFn Bases.spineUnit)

radians :: Floating a => a -> a
radians deg = deg * pi / 180

basePair
  :: (Floating a, Epsilon a, Ord a) => CSG.Scene a -> CSG.Scene a -> CSG.Scene a
basePair m m' =
  m'
    <> ( m
       & CSG.rotateAxisAngleScene (V3 0 0 1) (radians 180)
       & CSG.rotateAxisAngleScene (V3 0 1 0) (radians 180)
       )
    <> spineUnit
    <> ( spineUnit
       & CSG.rotateAxisAngleScene (V3 0 0 1) (radians 180)
       & CSG.rotateAxisAngleScene (V3 0 1 0) (radians 180)
       )

gcPair :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
gcPair = basePair guanine cytosine

cgPair :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
cgPair = basePair cytosine guanine

atPair :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
atPair = basePair adenine thymine

taPair :: (Floating a, Epsilon a, Ord a) => CSG.Scene a
taPair = basePair thymine adenine

placePairs :: (Floating a, Epsilon a) => [CSG.Scene a] -> CSG.Scene a
placePairs pairs = mconcat $ placePair <$> zip [0 :: Int ..] pairs
 where
  placePair (index, pair) =
    {-
    (pair
     & CSG.translateScene (V3 0 dy dz))
     & CSG.rotateAxisAngleScene (V3 0 0 1) rz
    -}
    (pair & CSG.rotateAxisAngleScene (V3 0 0 1) rz & CSG.translateScene
      (V3 0 dy dz)
    )
   where
    dy = -0.23 * fromIntegral index
    dz = 3.32 * fromIntegral index
    rz = radians (36.0 * fromIntegral index)

-- DNA (5'-D(*CP*GP*CP*GP*AP*AP*TP*TP*CP*GP*CP*G)-3')
testScene :: CSG.Scene Double
testScene =
  placePairs
    . reverse
    $ [ cgPair
      , gcPair
      , cgPair
      , gcPair
      , atPair
      , atPair
      , taPair
      , taPair
      , cgPair
      , gcPair
      , cgPair
      , gcPair
      ]
