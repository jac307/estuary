module Estuary.Help.Crudo where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
crudoHelpFile :: MonadWidget t m => m ()
crudoHelpFile = divClass "languageHelp" $ do
  about
  functionRef "trueno"
  functionRef "río"
  functionRef "cascada"
  functionRef "volcán"
  functionRef "rama"
  functionRef "viento"
  functionRef "eco"
  functionRef "oscuridad"
  functionRef "salvaje"
  functionRef "este"
  functionRef "oeste"
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Crudo"
 divClass "aboutText" $ text "A mini live coding esolang developed in Bogotá, Colombia."

exampleText :: String -> String

exampleText "trueno" = " \"trueno\""
exampleText "río" = " \"río\""
exampleText "cascada" = " \"cascada\""
exampleText "volcán" = " \"volcán\""
exampleText "rama" =  " \"rama\""
exampleText "viento" = " \"viento\""
exampleText "cueva" = " \"short\""
exampleText "eco" = " \"trueno\" eco 2"
exampleText "oscuridad" = " \"río\" oscuridad 2"
exampleText "salvaje" = " \"rama\" salvaje 3"
exampleText "este" = " \"viento\" este 2"
exampleText "oeste" = " \"cueva\" oeste 0.75"

referenceText :: String -> String

referenceText "trueno" = "returns Dirt's \"bd\" sample"
referenceText "río" = "returns Dirt's \"sn\" sample"
referenceText "cascada" = "returns Dirt's \"wind\" sample"
referenceText "volcán" = "returns Dirt's \"stomp\" sample"
referenceText "rama" = "returns Dirt's \"hh\" sample"
referenceText "viento" = "returns Dirt's \"wind\" sample"
referenceText "cueva" = "returns Dirt's \"short\" sample"
referenceText "eco" = "returns TidalCycles' palindrome"
referenceText "oscuridad" = "returns TidalCycles' slow"
referenceText "salvaje" = "returns TidalCycles' density"
referenceText "este" = "returns TidalCycles' fast"
referenceText "oeste" = "returns TidalCycles' trunc"

functionRef :: MonadWidget t m => String -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- divClass "refExampleButton" $ button x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText" $ text (exampleText x)
 hideableWidget referenceVisible "referenceText" $ text (referenceText x)
 return ()