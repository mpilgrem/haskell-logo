{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes       #-}

module Diagrams.TwoD.HaskellToolStackLogo
  ( haskellToolStackLogo
  ) where

import          Diagrams.Prelude
                  ( Any, LineCap (..), Path, QDiagram, Renderable, V2, (#)
                  , fillColor, fromVertices, lc, lineCap, lw, lwL
                  , none, p2, r2, roundedRect, sRGB24, scale, strokeLine
                  , translate, translateY, white
                  )
import          Diagrams.TwoD.HaskellLogo
                  ( haskellLogo', plainHaskellLogoPalette )

-- | Yields the Haskell Tool Stack logo with width 512 px and origin at bottom
-- left corner of the logo.
haskellToolStackLogo ::
     forall b. (Renderable (Path V2 Double) b)
  => QDiagram b V2 Double Any
haskellToolStackLogo =
  (  (  haskellLogo
     <> stack
     ) # translate (r2 (42, 118))
  <> background
  ) # scale (512/288)

 where
  haskellLogo = haskellLogo' (plainHaskellLogoPalette white)

  stack =
    (  stackLine (-21)
    <> stackLine (-51)
    <> stackLine (-81)
    ) # lc white # lwL 18 # lineCap LineCapRound

  background =
      roundedRect 288 288 72
    # translate (r2 (144, 144))
    # fillColor (sRGB24 126 86 194)
    # lw none

  stackLine ::
       Renderable (Path V2 Double) b1
    => Double
    -> QDiagram b1 V2 Double Any
  stackLine h =
      fromVertices (p2 <$> [(0, 0), (204, 0)])
    # strokeLine
    # translateY h
