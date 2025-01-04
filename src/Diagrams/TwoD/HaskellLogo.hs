{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}

module Diagrams.TwoD.HaskellLogo
  ( HaskellLogoPalette (..)
  , defaultHaskellLogoPalette
  , haskellLogo
  ) where

import          Data.List.NonEmpty ( NonEmpty (..) )
import          Diagrams.Prelude
                  ( Any, Color (..), Colour, Path, QDiagram, Renderable, V2, (#)
                  , fillColor, fromVertices, glueLine, lw, none, p2, r2, sRGB24
                  , scale, strokeLoop, translate
                  )

-- | Type representing palettes of colours for Haskell logos
data HaskellLogoPalette c = HaskellLogoPalette
  { arrowColor :: c
    -- ^ The colour of the 'arrow' element.
  , lambdaColor :: c
    -- ^ The colour of the 'lambda' element.
  , equalsColor :: c
    -- ^ The colour of the 'equals' element.
  } deriving Eq

-- | Given a palette of colours, yields the Haskell logo with width 1.0 and
-- origin at bottom left corner of the logo.
haskellLogo ::
     (Color c, Renderable (Path V2 Double) b)
  => HaskellLogoPalette c
  -> QDiagram b V2 Double Any
haskellLogo palette =
  (   arrow # fillColor (arrowColor palette)
  <> lambda # fillColor (lambdaColor palette)
  <> equals # fillColor (equalsColor palette)
  ) # scale (1/204) # lw none
 where
  fromPairs (p :| ps) =
    let ps' = p : ps <> [p]
    in  translate (r2 p) $ strokeLoop $ glueLine $ fromVertices $ p2 <$> ps'

  arrow = fromPairs
    [ (  0,   0)
    , ( 48,  72)
    , (  0, 144)
    , ( 36, 144)
    , ( 84,  72)
    , ( 36,   0)
    ]

  lambda = fromPairs
    [ ( 48,   0)
    , ( 96,  72)
    , ( 48, 144)
    , ( 84, 144)
    , (180,   0)
    , (144,   0)
    , (114,  45)
    , ( 84,   0)
    ]

  equals = equalsTop <> equalsBottom

  equalsTop = fromPairs
    [ (140,   78)
    , (124,  102)
    , (204,  102)
    , (204,   78)
    ]

  equalsBottom = fromPairs
    [ (164,  42)
    , (148,  66)
    , (204,  66)
    , (204,  42)
    ]

-- | The default palette of colours.
defaultHaskellLogoPalette :: HaskellLogoPalette (Colour Double)
defaultHaskellLogoPalette = HaskellLogoPalette
  { arrowColor = sRGB24 69 58 98
  , lambdaColor = sRGB24 94 80 134
  , equalsColor = sRGB24 143 78 139
  }
