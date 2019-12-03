{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.CineCer0.CineCer0State where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHCJS.Types
import GHCJS.DOM.Types (HTMLDivElement)
import GHCJS.Marshal.Pure
import Data.IntMap.Strict as IntMap
import Data.Time
import TextShow
import Control.Monad

import Estuary.Types.Tempo
import Estuary.Languages.CineCer0.Parser
import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.PositionAndRate

newtype CineCer0Video = CineCer0Video { videoJSVal :: JSVal }

instance PToJSVal CineCer0Video where pToJSVal (CineCer0Video val) = val

instance PFromJSVal CineCer0Video where pFromJSVal = CineCer0Video

foreign import javascript safe
  "var video = document.createElement('video'); video.setAttribute('src',$1); $r=video; video.loop = true;"
  makeVideo :: Text -> IO CineCer0Video

foreign import javascript safe
  "$2.appendChild($1); $1.play();"
  appendVideo :: CineCer0Video -> HTMLDivElement -> IO ()

foreign import javascript safe
  "$1.removeChild($2)"
  removeVideo :: HTMLDivElement -> CineCer0Video -> IO ()

foreign import javascript safe
  "$1.style = $2;"
  videoStyle_ :: CineCer0Video -> Text -> IO ()

foreign import javascript unsafe
  "$1.muted = true;"
  muteVideo :: CineCer0Video -> IO ()

foreign import javascript unsafe
  "$1.videoWidth"
  videoWidth :: CineCer0Video -> IO Double

foreign import javascript unsafe
  "$1.videoHeight"
  videoHeight :: CineCer0Video -> IO Double

videoStyle :: CineCer0Video -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> IO ()
videoStyle v x y w h o bl br c g s = videoStyle_ v $ "left: " <> showt x <> "px; top: " <> showt y <> "px; position: absolute; width:" <> showt w <> "px; height:" <> showt h <> "px; object-fit: fill; opacity: " <> showt o <> "%; filter:blur( " <> showt bl <> "px) " <> "brightness( " <> showt br <> "%) " <> "contrast( " <> showt c <> "%) " <> "grayscale( " <> showt g <> "%) " <> "saturate( " <> showt s <> ");"

----  Rate and Position ----

foreign import javascript unsafe
  "$1.playbackRate = $2;"
  videoPlaybackRate :: CineCer0Video -> Double -> IO ()

foreign import javascript unsafe
  "$1.currentTime = $2;"
  videoPlaybackPosition :: CineCer0Video -> Double -> IO ()

foreign import javascript unsafe
  "$1.duration"
  getLengthOfVideo :: CineCer0Video -> IO Double

addVideo :: HTMLDivElement -> VideoSpec -> IO CineCer0Video
addVideo j spec = do
  let url = T.pack $ sampleVideo spec
  x <- makeVideo url
  muteVideo x
  appendVideo x j
  return x

updateCineCer0State :: Tempo -> UTCTime -> CineCer0Spec -> CineCer0State -> IO CineCer0State
updateCineCer0State t now spec st = do
  divWidth <- offsetWidth $ videoDiv st
  divHeight <- offsetHeight $ videoDiv st
  -- add or delete videos
  let newVideoSpecs = difference spec (videos st) -- :: IntMap VideoSpec
  let toAdd = IntMap.filter (\x -> sampleVideo x /= "") newVideoSpecs
  addedVideos <- mapM (addVideo $ videoDiv st) toAdd -- :: IntMap CineCer0Video
  let videosWithRemovedSpecs = difference (videos st) spec -- :: IntMap CineCer0Video
  let videosWithEmptySource = intersection (videos st) $ IntMap.filter (\x -> sampleVideo x == "") spec -- :: IntMap CineCer0Video
  let toDelete = union videosWithRemovedSpecs videosWithEmptySource
  mapM (removeVideo $ videoDiv st) toDelete
  let videosThereBefore = difference (videos st) toDelete -- :: IntMap CineCer0Video
  let continuingVideos = union videosThereBefore addedVideos -- :: IntMap CineCer0Video
  sequence $ intersectionWith (updateContinuingVideo t now (divWidth,divHeight)) spec continuingVideos
  return $ st { videos = continuingVideos }

updateContinuingVideo :: Tempo -> UTCTime -> (Double,Double) -> VideoSpec -> CineCer0Video -> IO ()
updateContinuingVideo t now (sw,sh) s v = do
  -- need fitWidth and fitHeight to be some representation of "maximal fit"
  vw <- videoWidth v
  vh <- videoHeight v
  when (vw /= 0 && vh /= 0) $ do
    let aspectRatio = vw/vh
    let heightIfFitsWidth = sw / aspectRatio
    let widthIfFitsHeight = sh * aspectRatio
    let fitByWidth = heightIfFitsWidth <= sh
    let fitWidth = if fitByWidth then sw else widthIfFitsHeight
    let fitHeight = if fitByWidth then heightIfFitsWidth else sh
    let actualWidth = (realToFrac $ width s) * fitWidth
    let actualHeight = (realToFrac $ height s) * fitHeight
    let centreX = (realToFrac $ posX s * 0.5 + 0.5) * sw
    let centreY = (realToFrac $ posY s * 0.5 + 0.5) * sh
    let leftX = centreX - (actualWidth * 0.5)
    let topY = sh - (centreY + (actualHeight * 0.5))
    -- update playback rate
    lengthOfVideo <- realToFrac <$> getLengthOfVideo v
    let rate = (playbackRate s) t lengthOfVideo now
    maybe (return ()) (videoPlaybackRate v) $ fmap realToFrac rate
    -- update position in time
    let pos = (playbackPosition s) t lengthOfVideo now
    maybe (return ()) (videoPlaybackPosition v) $ fmap realToFrac pos
    -- update opacity
    --let opacidad = (opacity s) t lengthOfVideo now
    -- update geometry/appearance/etc
    videoStyle v (floor $ leftX) (floor $ topY) (floor $ actualWidth) (floor $ actualHeight) (floor (opacity s)) (floor (blur s)) (floor (brightness s)) (floor (contrast s)) (floor (grayscale s)) (realToFrac (saturate s))


emptyCineCer0State :: HTMLDivElement -> CineCer0State
emptyCineCer0State j = CineCer0State {
  videoDiv = j,
  videos = empty
  }

data CineCer0State = CineCer0State {
  videoDiv :: HTMLDivElement,
  videos :: IntMap CineCer0Video
  }

foreign import javascript unsafe
  "$1.offsetWidth"
  offsetWidth :: HTMLDivElement -> IO Double

foreign import javascript unsafe
  "$1.offsetHeight"
  offsetHeight :: HTMLDivElement -> IO Double
