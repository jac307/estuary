module Estuary.Languages.CineCer0.Parser (escuchar,CineCer0Spec) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time

import Estuary.Languages.ExpParser
import Estuary.Languages.CineCer0.VideoSpec

type CineCer0Spec = IntMap VideoSpec

escuchar :: String -> Either String CineCer0Spec
escuchar s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runExpParser cineCer0Spec x
    f (ParseFailed l s) = Left s

cineCer0Spec :: ExpParser CineCer0Spec
cineCer0Spec = fmap (fromList . zip [0..]) $ listOfDoStatements videoSpec

videoSpec :: ExpParser VideoSpec
videoSpec =
  literalVideoSpec <|>
  int_VideoSpec <*> int <|>
  videoSpec_videoSpec <*> videoSpec

int :: ExpParser Int
int = fromIntegral <$> integer

nominalDiffTime :: ExpParser NominalDiffTime
nominalDiffTime = fromRational <$> rationalOrInteger

literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec =
  fmap stringToVideoSpec string <|>
  fmap emptyVideoSpec string

videoSpec_int_videoSpec :: ExpParser (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSourceNumber <$ reserved ":"

int_VideoSpec :: ExpParser (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

rat_videoSpec_videoSpec :: ExpParser (Rational -> VideoSpec -> VideoSpec)
rat_videoSpec_videoSpec =
  playNatural <$ reserved "natural" <|> --time function
  playRound <$ reserved "round" <|> -- time function
  playRoundMetre <$ reserved "roundMetre" <|> -- time function
  setWidth <$ reserved "w" <|>
  setHeight <$ reserved "h" <|>
  setPosX <$ reserved "posX" <|>
  setPosY <$ reserved "posY" <|>

  setOpacity <$ (reserved "opacity" <|> reserved "tempestuoso" <|> reserved " tempestuosa" <|> reserved "tempestuosas" <|> reserved "tempestuosos" <|> reserved "transformado" <|> reserved "transformados" <|> reserved "transformada" <|> reserved "transformadas") <|>

  setBlur <$ (reserved "blur" <|> reserved "consciente" <|> reserved "conscientes" <|> reserved "extraño" <|> reserved "extraña" <|> reserved "extraños" <|> reserved "extrañas" <|> reserved "diferente" <|> reserved "diferentes" <|> reserved "fuerte" <|> reserved "fuertes") <|>

  setBrightness <$ (reserved "brightness" <|> reserved "cariñosamente" <|> reserved "cuidadosamente" <|> reserved "suavemente" <|> reserved "conscientemente" <|> reserved "profundamente") <|>

  setContranst <$ (reserved "contrast" <|> reserved "lejanas" <|> reserved "lejanos" <|> reserved "cercanos" <|> reserved "cercanas" <|> reserved "blanquecinas" <|> reserved "oscuros" <|> reserved "oscuras" <|> reserved "débil" <|> reserved "débiles") <|>

  setGrayscale <$ (reserved "grayscale" <|> reserved "despacio" <|> reserved "deprisa" <|> reserved "claramente" <|> reserved "cómodamente" <|> reserved "rápidamente" <|> reserved "cruelmente") <|>

  setSaturate <$ (reserved "saturate" <|> reserved "lejos"<|> reserved "aquí" <|> reserved "ahí" <|> reserved "allí" <|> reserved "adentro" <|> reserved "cerca" <|> reserved "detrás" <|> reserved "adelante" <|> reserved "debajo")

rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_videoSpec_videoSpec =
  playEvery <$ reserved "every" <|> --time function
  setPosCoord <$ (reserved "pos" <|> reserved "sentada" <|> reserved "escuchar" <|> reserved "escucho" <|> reserved "escuchando" <|> reserved "escucharé" <|> reserved "escuché" <|> reserved "suena" <|> reserved "gustan" <|> reserved "recuerdo" <|> reserved "recordando" <|> reserved "recordaré" <|> reserved "recordé" <|> reserved "atraviesan" <|> reserved "atravesando" <|> reserved  "atravesé" <|> reserved "atravesaré" <|> reserved "hago" <|> reserved "hacen" <|> reserved "haré") <|>

  setSize <$ (reserved "size" <|> reserved "nunca" <|> reserved "mañana" <|> reserved "ayer" <|> reserved "anoche" <|> reserved "siempre" <|> reserved "seguido" <|> reserved "aún" <|> reserved "pronto" <|> reserved "frecuentemente" <|> reserved "hoy" <|> reserved "antes")

rat_rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_videoSpec_videoSpec =
  playChop' <$ reserved "chop'" -- time function

rat_rat_rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_rat_videoSpec_videoSpec =
  playChop <$ reserved "chop" -- time function

nd_rat_videoSpec_videoSpec :: ExpParser (NominalDiffTime -> Rational -> VideoSpec -> VideoSpec)
nd_rat_videoSpec_videoSpec = playNow <$ reserved "now" -- time function

nd_nd_rat_rat_videoSpec_videoSpec :: ExpParser (NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec)
nd_nd_rat_rat_videoSpec_videoSpec = playChopSecs <$ reserved "chopSecs" -- time function

videoSpec_videoSpec :: ExpParser (VideoSpec -> VideoSpec)
videoSpec_videoSpec =
  --string_VideoSpec_VideoSpec <*> string <|> --mask function
  rat_videoSpec_videoSpec <*> rationalOrInteger <|> -- time function
  rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <|> -- pos function
  rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  rat_rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  nd_rat_videoSpec_videoSpec <*> nominalDiffTime <*> rationalOrInteger <|> -- time function
  nd_nd_rat_rat_videoSpec_videoSpec <*> nominalDiffTime <*> nominalDiffTime <*> rationalOrInteger <*> rationalOrInteger -- time function
