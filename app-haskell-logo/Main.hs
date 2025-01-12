module Main
  ( main
  ) where

import          Diagrams.Backend.SVG.CmdLine ( B, mainWith )
import          Diagrams.Prelude ( Diagram )
import          Diagrams.TwoD.HaskellLogo
                  ( defaultHaskellLogoPalette, haskellLogo )

main :: IO ()
main = mainWith (haskellLogo defaultHaskellLogoPalette :: Diagram B)
