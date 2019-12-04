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
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Linear.V3                      ( V3(V3) )

spineUnit :: Fractional a => [Atom a]
spineUnit = []

prependName :: Text -> Atom a -> Atom a
prependName prefix (Atom name el p) = Atom (prefix <> name) el p

guanine :: Fractional a => [Atom a]
guanine =
  prependName "G-"
    <$> [ Atom "C1'" Carbon   (V3 -2.477 5.399 0.000)
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

cytosine :: Fractional a => [Atom a]
cytosine =
  prependName "C-"
    <$> [ Atom "C1'" Carbon   (V3 -2.477 5.402 0.000)
        , Atom "N1"  Nitrogen (V3 -1.285 4.542 0.000)
        , Atom "C2"  Carbon   (V3 -1.472 3.158 0.000)
        , Atom "O2"  Oxygen   (V3 -2.628 2.709 0.001)
        , Atom "N3"  Nitrogen (V3 -0.391 2.344 0.000)
        , Atom "C4"  Carbon   (V3 0.837 2.868 0.000)
        , Atom "N4"  Nitrogen (V3 1.875 2.027 0.001)
        , Atom "C5"  Carbon   (V3 1.056 4.275 0.000)
        , Atom "C6"  Carbon   (V3 -0.023 5.068 0.000)
        ]

adenine :: Fractional a => [Atom a]
adenine =
  prependName "A-"
    <$> [ Atom "C1'" Carbon   (V3 -2.479 5.346 0.000)
        , Atom "N9"  Nitrogen (V3 -1.291 4.498 0.000)
        , Atom "C8"  Carbon   (V3 0.024 4.897 0.000)
        , Atom "N7"  Nitrogen (V3 0.877 3.902 0.000)
        , Atom "C5"  Carbon   (V3 0.071 2.771 0.000)
        , Atom "C6"  Carbon   (V3 0.369 1.398 0.000)
        , Atom "N6"  Nitrogen (V3 1.611 0.909 0.000)
        , Atom "N1"  Nitrogen (V3 -0.668 0.532 0.000)
        , Atom "C2"  Carbon   (V3 -1.912 1.023 0.000)
        , Atom "N3"  Nitrogen (V3 -2.320 2.290 0.000)
        , Atom "C4"  Carbon   (V3 -1.267 3.124 0.000)
        ]

thymine :: Fractional a => [Atom a]
thymine =
  prependName "T-"
    <$> [ Atom "C1'" Carbon   (V3 -2.481 5.354 0.000)
        , Atom "N1"  Nitrogen (V3 -1.284 4.500 0.000)
        , Atom "C2"  Carbon   (V3 -1.462 3.135 0.000)
        , Atom "O2"  Oxygen   (V3 -2.562 2.608 0.000)
        , Atom "N3"  Nitrogen (V3 -0.298 2.407 0.000)
        , Atom "C4"  Carbon   (V3 0.994 2.897 0.000)
        , Atom "O4"  Oxygen   (V3 1.944 2.119 0.000)
        , Atom "C5"  Carbon   (V3 1.106 4.338 0.000)
        , Atom "C5M" Carbon   (V3 2.466 4.961 0.001)
        , Atom "C6"  Carbon   (V3 -0.024 5.057 0.000)
        ]
