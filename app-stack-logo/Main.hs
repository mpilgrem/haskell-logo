module Main
  ( main
  ) where

import          Diagrams.Backend.SVG.CmdLine ( B, mainWith )
import          Diagrams.Prelude ( Diagram )
import          Diagrams.TwoD.HaskellToolStackLogo ( haskellToolStackLogo )

main :: IO ()
main = mainWith (haskellToolStackLogo :: Diagram B)
