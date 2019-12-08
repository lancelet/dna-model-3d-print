{-# LANGUAGE NegativeLiterals #-}
module Main where

import qualified DNAModel.Bases                as Bases
import qualified DNAModel.Atoms                as Atoms
import qualified DNAModel.CSG.Types            as CSG
import qualified DNAModel.CSG.RhinoExport      as R
import qualified DNAModel.TestModel            as TestModel

import           Data.Function                  ( (&) )
import qualified Data.Text                     as Text
import           Linear.V3                      ( V3(V3) )

main :: IO ()
main = do
  let
    filamentCfg = Atoms.GeomConfig
      { Atoms.gcEpsilon             = 0.2 :: Double
      , Atoms.gcConnectorRadius     = 0.3
      , Atoms.gcConnectorDepth      = 0.3
      , Atoms.gcChamfer             = 0.08
      , Atoms.gcHydrogenSocketDepth = 0.125
      , Atoms.gcHydrogenPlugDepth   = 0.1
      }
    sizeFn e = Atoms.elementRadius e * (1.0 / 0.7)

    createFilamentScene rotDegNY bases =
      CSG.Scene (Atoms.proteinGeom filamentCfg sizeFn bases)
        & CSG.rotateAxisAngleScene (V3 0 -1 0) (rotDegNY * pi / 180)
        & CSG.scaleScene 10

    sceneGF = createFilamentScene 0.0 Bases.guanine
    sceneCF = createFilamentScene 0.0 Bases.cytosine
    sceneAF = createFilamentScene 0.0 Bases.adenine
    sceneTF = createFilamentScene 0.0 Bases.thymine
    sceneDF = createFilamentScene 0.0 Bases.spineUnit

    plug    = CSG.Scene
      [Atoms.defaultHydrogenPlugGeom filamentCfg (sizeFn Atoms.Hydrogen)]

    testModel = TestModel.testScene

  writeFile "guanine.filament.rhino.py"  (Text.unpack $ R.scene sceneGF)
  writeFile "cytosine.filament.rhino.py" (Text.unpack $ R.scene sceneCF)
  writeFile "adenine.filament.rhino.py"  (Text.unpack $ R.scene sceneAF)
  writeFile "thymine.filament.rhino.py"  (Text.unpack $ R.scene sceneTF)
  writeFile "deoxy.filament.rhino.py"    (Text.unpack $ R.scene sceneDF)

  writeFile "testmodel.rhino.py"         (Text.unpack $ R.scene testModel)

  writeFile "testplug.rhino.py"          (Text.unpack $ R.scene plug)
