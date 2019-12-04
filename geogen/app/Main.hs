module Main where

import qualified DNAModel.Bases                as Bases
import qualified DNAModel.Atoms                as Atoms
import qualified DNAModel.CSG.Types            as CSG
import qualified DNAModel.CSG.RhinoExport      as R

import qualified Data.Text                     as Text

main :: IO ()
main = do
  let
    filamentCfg = Atoms.GeomConfig { Atoms.gcEpsilon         = 0.2 :: Double
                                   , Atoms.gcConnectorRadius = 0.285
                                   , Atoms.gcConnectorDepth  = 0.5
                                   , Atoms.gcChamfer         = 0.05
                                   }
    sizeFn e = Atoms.elementRadius e * (1.0 / 0.7)

    createFilamentScene bases = CSG.scaleScene
      1
      (CSG.Scene (Atoms.proteinGeom filamentCfg sizeFn bases))

    sceneGF = createFilamentScene Bases.guanine
    sceneCF = createFilamentScene Bases.cytosine
    sceneAF = createFilamentScene Bases.adenine
    sceneTF = createFilamentScene Bases.thymine
    sceneDF = createFilamentScene Bases.spineUnit

  writeFile "guanine.filament.rhino.py"  (Text.unpack $ R.scene sceneGF)
  writeFile "cytosine.filament.rhino.py" (Text.unpack $ R.scene sceneCF)
  writeFile "adenine.filament.rhino.py"  (Text.unpack $ R.scene sceneAF)
  writeFile "thymine.filament.rhino.py"  (Text.unpack $ R.scene sceneTF)
  writeFile "deoxy.filament.rhino.py"    (Text.unpack $ R.scene sceneDF)
