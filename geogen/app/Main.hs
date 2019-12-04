module Main where

import qualified DNAModel.Bases                as Bases
import qualified DNAModel.Atoms                as Atoms
import qualified DNAModel.CSG.Types            as CSG
import qualified DNAModel.CSG.RhinoExport      as R

import qualified Data.Text                     as Text

main :: IO ()
main = do
  let cfg = Atoms.GeomConfig { Atoms.gcEpsilon         = 0.2 :: Double
                             , Atoms.gcConnectorRadius = 0.2
                             , Atoms.gcConnectorDepth  = 0.3
                             , Atoms.gcChamfer         = 0.05
                             }
      sizeFn e = Atoms.elementRadius e * (1.0 / 0.7)

      sceneG = CSG.Scene (Atoms.proteinGeom cfg sizeFn Bases.guanine)
      sceneC = CSG.Scene (Atoms.proteinGeom cfg sizeFn Bases.cytosine)
      sceneA = CSG.Scene (Atoms.proteinGeom cfg sizeFn Bases.adenine)
      sceneT = CSG.Scene (Atoms.proteinGeom cfg sizeFn Bases.thymine)
      sceneD = CSG.Scene (Atoms.proteinGeom cfg sizeFn Bases.spineUnit)

  writeFile "guanine.rhino.py"  (Text.unpack $ R.scene sceneG)
  writeFile "cytosine.rhino.py" (Text.unpack $ R.scene sceneC)
  writeFile "adenine.rhino.py"  (Text.unpack $ R.scene sceneA)
  writeFile "thymine.rhino.py"  (Text.unpack $ R.scene sceneT)
  writeFile "deoxy.rhino.py"    (Text.unpack $ R.scene sceneD)
