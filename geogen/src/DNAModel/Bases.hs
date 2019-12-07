{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
module DNAModel.Bases where

import           DNAModel.Atoms                 ( Atom(Atom)
                                                , Output(Output, Dummy)
                                                , Element
                                                  ( Hydrogen
                                                  , Carbon
                                                  , Nitrogen
                                                  , Oxygen
                                                  , Phosphorus
                                                  )
                                                )

import           Data.Text                      ( Text )
import           Linear.V3                      ( V3(V3) )

prependName :: Text -> Atom a -> Atom a
prependName prefix (Atom o name el p) = Atom o (prefix <> name) el p

translate :: Num a => V3 a -> [Atom a] -> [Atom a]
translate ofs atoms = translateOneAtom <$> atoms
  where translateOneAtom (Atom o name el p) = Atom o name el (p + ofs)

spineUnit :: Fractional a => [Atom a]
spineUnit =
  prependName "DeoxyP-"
    <$> [ Atom Output "C2'"    Carbon     (V3 -2.251 8.158 -1.870)
        , Atom Output "C3'"    Carbon     (V3 -3.188 6.244 0.714)
        , Atom Output "C4'"    Carbon     (V3 -3.327 7.182 -1.455)
        , Atom Output "C5'"    Carbon     (V3 -3.903 7.343 -0.062)
        , Atom Output "O4'"    Oxygen     (V3 -2.807 5.842 -1.523)
        , Atom Output "O5'"    Oxygen     (V3 -5.321 7.208 -0.059)
        , Atom Output "OP1"    Oxygen     (V3 -6.983 6.548 1.641)
        , Atom Output "OP2"    Oxygen     (V3 -5.241 8.252 2.216)
        , Atom Output "OP3"    Oxygen     (V3 -7.108 8.874 0.649)
        , Atom Output "P"      Phosphorus (V3 -6.171 7.830 1.148)
        , Atom Dummy  "H1"     Hydrogen   (V3 -1.897 7.987 -2.888)
        , Atom Dummy  "H2"     Hydrogen   (V3 -3.278 8.784 -1.860)
        , Atom Dummy  "H3"     Hydrogen   (V3 -4.187 7.255 -2.102)
        , Atom Dummy  "H4"     Hydrogen   (V3 -3.533 8.268 0.452)
        , Atom Dummy  "H5"     Hydrogen   (V3 -2.251 6.541 1.168)
        , Atom Dummy  "H6"     Hydrogen   (V3 -3.856 5.840 1.488)
        , Atom Dummy  "DummyC" Carbon     (V3 -2.465 5.399 -0.246)
        , Atom Dummy  "DummyC" Carbon     (V3 -6.48106 5.09078 1.45)
        , Atom Dummy  "DummyO" Oxygen     (V3 -1.80055 9.63195 -1.679)
        ]

guanine :: Fractional a => [Atom a]
guanine =
  prependName "G-"
    <$> [ Atom Output "C1'"    Carbon   (V3 -2.477 5.399 0.000)
        , Atom Output "N9"     Nitrogen (V3 -1.289 4.551 0.000)
        , Atom Output "C8"     Carbon   (V3 0.023 4.962 0.000)
        , Atom Output "N7"     Nitrogen (V3 0.870 3.969 0.000)
        , Atom Output "C5"     Carbon   (V3 0.071 2.833 0.000)
        , Atom Output "C6"     Carbon   (V3 0.424 1.460 0.000)
        , Atom Output "O6"     Oxygen   (V3 1.554 0.955 0.000)
        , Atom Output "N1"     Nitrogen (V3 -0.700 0.641 0.000)
        , Atom Output "C2"     Carbon   (V3 -1.999 1.087 0.000)
        , Atom Output "N2"     Nitrogen (V3 -2.949 0.139 -0.001)
        , Atom Output "N3"     Nitrogen (V3 -2.342 2.364 0.001)
        , Atom Output "C4"     Carbon   (V3 -1.265 3.177 0.000)
        , Atom Dummy  "H1"     Hydrogen (V3 2.013 3.975 0.000)
        , Atom Dummy  "H2"     Hydrogen (V3 -3.300 4.538 0.000)
        , Atom Dummy  "H3"     Hydrogen (V3 -3.988 0.613 0.000)
        , Atom Dummy  "DummyO" Oxygen   (V3 -2.944 5.842 -1.237)
        , Atom Dummy  "DummyC" Carbon   (V3 -3.101 6.244 1.027)
        ]

cytosine :: Fractional a => [Atom a]
cytosine =
  prependName "C-"
    <$> [ Atom Output "C1'"    Carbon   (V3 -2.477 5.402 0.000)
        , Atom Output "N1"     Nitrogen (V3 -1.285 4.542 0.000)
        , Atom Output "C2"     Carbon   (V3 -1.472 3.158 0.000)
        , Atom Output "O2"     Oxygen   (V3 -2.628 2.709 0.001)
        , Atom Output "N3"     Nitrogen (V3 -0.391 2.344 0.000)
        , Atom Output "C4"     Carbon   (V3 0.837 2.868 0.000)
        , Atom Output "N4"     Nitrogen (V3 1.875 2.027 0.001)
        , Atom Output "C5"     Carbon   (V3 1.056 4.275 0.000)
        , Atom Output "C6"     Carbon   (V3 -0.023 5.068 0.000)
        , Atom Dummy  "H1"     Hydrogen (V3 2.925 1.938 0.000)
        , Atom Dummy  "H2"     Hydrogen (V3 2.088 4.788 0.000)
        , Atom Dummy  "H3"     Hydrogen (V3 0.088 6.213 0.000)
        , Atom Dummy  "H4"     Hydrogen (V3 -3.300 4.538 0.00)
        , Atom Dummy  "DummyO" Oxygen   (V3 -2.944 5.842 -1.237)
        , Atom Dummy  "DummyC" Carbon   (V3 -3.101 6.244 1.027)
        ]

adenine :: Fractional a => [Atom a]
adenine =
  prependName "A-"
    <$> (translate
          ((V3 -2.477 5.399 0.000) - (V3 -2.479 5.346 0.000))
          [ Atom Output "C1'" Carbon   (V3 -2.479 5.346 0.000)
          , Atom Output "N9"  Nitrogen (V3 -1.291 4.498 0.000)
          , Atom Output "C8"  Carbon   (V3 0.024 4.897 0.000)
          , Atom Output "N7"  Nitrogen (V3 0.877 3.902 0.000)
          , Atom Output "C5"  Carbon   (V3 0.071 2.771 0.000)
          , Atom Output "C6"  Carbon   (V3 0.369 1.398 0.000)
          , Atom Output "N6"  Nitrogen (V3 1.611 0.909 0.000)
          , Atom Output "N1"  Nitrogen (V3 -0.668 0.532 0.000)
          , Atom Output "C2"  Carbon   (V3 -1.912 1.023 0.000)
          , Atom Output "N3"  Nitrogen (V3 -2.320 2.290 0.000)
          , Atom Output "C4"  Carbon   (V3 -1.267 3.124 0.000)
          ]
        )
    ++  [ Atom Dummy "DummyO" Oxygen   (V3 -2.944 5.842 -1.237)
        , Atom Dummy "DummyC" Carbon   (V3 -3.101 6.244 1.027)
        , Atom Dummy "H1"     Hydrogen (V3 -3.401 4.770 -0.002)
        , Atom Dummy "H2"     Hydrogen (V3 0.293 6.048 0.056)
        , Atom Dummy "H3"     Hydrogen (V3 2.363 1.543 -0.057)
        ]

thymine :: Fractional a => [Atom a]
thymine =
  prependName "T-"
    <$> (translate
          ((V3 -2.477 5.399 0.000) - (V3 -2.481 5.354 0.000))
          [ Atom Output "C1'" Carbon   (V3 -2.481 5.354 0.000)
          , Atom Output "N1"  Nitrogen (V3 -1.284 4.500 0.000)
          , Atom Output "C2"  Carbon   (V3 -1.462 3.135 0.000)
          , Atom Output "O2"  Oxygen   (V3 -2.562 2.608 0.000)
          , Atom Output "N3"  Nitrogen (V3 -0.298 2.407 0.000)
          , Atom Output "C4"  Carbon   (V3 0.994 2.897 0.000)
          , Atom Output "O4"  Oxygen   (V3 1.944 2.119 0.000)
          , Atom Output "C5"  Carbon   (V3 1.106 4.338 0.000)
          , Atom Output "C5M" Carbon   (V3 2.466 4.961 0.001)
          , Atom Output "C6"  Carbon   (V3 -0.024 5.057 0.000)
          ]
        )
    ++  [ Atom Dummy "DummyO" Oxygen   (V3 -2.944 5.842 -1.237)
        , Atom Dummy "DummyC" Carbon   (V3 -3.101 6.244 1.027)
        , Atom Dummy "H1"     Hydrogen (V3 -3.397 4.876 -0.047)
        , Atom Dummy "H2"     Hydrogen (V3 0.051 6.260 -0.004)
        , Atom Dummy "H3"     Hydrogen (V3 2.377 6.146 -0.008)
        , Atom Dummy "H4"     Hydrogen (V3 3.003 4.742 -0.906)
        , Atom Dummy "H5"     Hydrogen (V3 3.028 4.737 0.858)
        ]
