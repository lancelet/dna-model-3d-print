{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
module DNAModel.Bases where

import           DNAModel.Atoms                 ( Atom(Atom)
                                                , Element
                                                  ( Carbon
                                                  , Nitrogen
                                                  , Oxygen
                                                  )
                                                )
import qualified DNAModel.Atoms                as Atoms
import qualified DNAModel.CSG.Types            as CSG
import qualified DNAModel.CSG.RhinoExport      as R

import           Data.Function                  ( (&) )
import qualified Data.Text                     as Text
import           Linear.V3                      ( V3(V3) )

test :: IO ()
test = do
  let cfg = Atoms.GeomConfig (0.2 :: Double) 0.2 0.3 0.05
  let sizeFn e = Atoms.elementRadius e * (1 / 0.70)
  let geos  = Atoms.proteinGeom cfg sizeFn guanine
  let scene = CSG.Scene geos
  putStrLn $ Text.unpack $ R.scene scene

guanine :: Fractional a => [Atom a]
guanine =
  [ Atom "C1'" Carbon   (V3 -2.477 5.399 0.000)
  , Atom "N9"  Nitrogen (V3 -1.289 4.551 0.000)
  , Atom "C8"  Carbon   (V3 0.023 4.962 0.000)
  , Atom "N7"  Nitrogen (V3 0.870 3.969 0.000)
  , Atom "C5"  Carbon   (V3 0.071 2.833 0.000)
  , Atom "C6"  Carbon   (V3 0.424 1.460 0.000)
  , Atom "O6"  Oxygen   (V3 1.554 0.955 0.000)
  , Atom "N1"  Nitrogen (V3 -0.700 0.641 0.000)
  , Atom "C2"  Carbon   (V3 -1.999 1.087 0.000)
  , Atom "N2"  Nitrogen (V3 -2.949 0.139 -0.001)
  , Atom "N3"  Nitrogen (V3 -2.342 2.364 0.001)
  , Atom "C4"  Carbon   (V3 -1.265 3.177 0.000)
  ]
