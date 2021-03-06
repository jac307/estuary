{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.EnsembleStatus where
import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
-- import Data.Tuple.Select
import Control.Monad.Trans (liftIO)
import Data.Map.Strict
import Control.Monad
import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleRequest
import Estuary.Types.Participant
import Estuary.Widgets.Editor
import Estuary.Widgets.Generic
import qualified Estuary.Types.Term as Term

ensembleStatusWidget :: MonadWidget t m => Editor t m (Event t EnsembleRequest)
ensembleStatusWidget = divClass "ensembleStatusWidget" $ do
  ctx <- context
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let uHandle = fmap userHandle ensC -- Dynamic Text
  ensName <- holdUniqDyn $ fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let status = fmap wsStatus ctx --ensemble status
  anonymous <- holdUniqDyn $ fmap anonymousParticipants ens -- Dynamic t Int

  -- divClass "ensemble-name-container code-font" $ do
  --   divClass "ensemble-name" $ do
  --     term Term.Ensemble >>= dynText
  --     text ": "
  --     dynText ensName

  divClass "statusWidgetScrollableContainer" $ do
    divClass "infoContainer" $ do
      divClass "tableContainer code-font" $ do
        status <- el "table" $ do
          now <- liftIO getCurrentTime -- this time is measured before building the widget
          evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
          currentTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
          rec
            c1 <- count (switchDyn $ fmap (leftmost . fmap fst . elems) $ m1) -- count :: Num b => Event a -> m (Dynamic b)
            c2 <- count (switchDyn $ fmap (leftmost . elems) $ m2)
            c3 <- count (switchDyn $ fmap (leftmost . elems) $ m3)
            let c' = fmap (`mod` 3) (c1 + c2 + c3) -- event 0,1,2
            -- hideableWidget' :: MonadWidget t m => Dynamic t Bool -> m a -> m a
            -- listWithKey :: forall t k v m a. (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
            -- switchDyn :: forall t a. Reflex t => Dynamic t (Event t a) -> Event t a
            m1 <- hideableWidget' (fmap (== 0) c') $ do
              headerMode1 ensName
              (listWithKey ensParticipants $ (mode1 uHandle currentTime))
            m2 <- hideableWidget' (fmap (== 1) c') $ do
              headerMode2 ensName
              (listWithKey ensParticipants $ (mode2 uHandle currentTime))
            m3 <- hideableWidget' (fmap (== 2) c') $ do
              headerMode3 ensName
              (listWithKey ensParticipants $ (mode3 uHandle currentTime))
          return $ switchDyn $ fmap (leftmost . fmap snd . elems) $ m1 --Event t EnsembleRequest -- Dymaic Map Int (Event t EnsembleRequest)

        divClass "statusWidgetAnonymousPart code-font" $ do
          term Term.AnonymousParticipants >>= dynText
          text ": "
          dynText $ fmap showt anonymous

        return status -- puts the value on the monad

headerMode1 :: MonadWidget t m  => Dynamic t Text -> m ()
headerMode1 ensName = divClass "rowContainer" $ do
  el "tr" $ do
    elClass "th" "statusWidgetNameAndLocation" $ divClass "statusWidgetPopupChild" $ dynText $ (constDyn "Ensemble: ") <> ensName
    elClass "th" "statusWidgetActivity" $ text "activity"
    elClass "th" "statusWidgetStatusInput" $ text "status"

headerMode2 :: MonadWidget t m  => Dynamic t Text -> m ()
headerMode2 ensName = divClass "rowContainer" $ do
  el "tr" $ do
    elClass "th" "statusWidgetName" $ dynText $ (constDyn "Ensemble: ") <> ensName
    elClass "th" "statusWidgetFPSAndLatency" $ text "latency"
    elClass "th" "statusWidgetFPSAndLatency" $ text "load"
    elClass "th" "statusWidgetFPSAndLatency" $ text "FPS"
    elClass "th" "statusWidgetFPSAndLatency" $ text "IP address"

headerMode3 :: MonadWidget t m  => Dynamic t Text -> m ()
headerMode3 ensName = divClass "rowContainer" $ do
  el "tr" $ do
    elClass "th" "statusWidgetName" $ dynText $ (constDyn "Ensemble: ") <> ensName

mode1 ::  MonadWidget t m  => Dynamic t Text -> Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t (), Event t EnsembleRequest)
mode1 uHandle t name part =  divClass "rowContainer" $ do
  ev <- clickableTr' $ do
    elClass "td" "statusWidgetNameAndLocation" $ participantNameLocationAndIPWidget name part
    elClass "td" "statusWidgetActivity" $ participantActivityWidget t name part
  status <- elClass "td" "statusWidgetStatusInput" $ participantStatusWidget uHandle name part
  return (ev, status)
  -- return (ev, status)


mode2 ::  MonadWidget t m  => Dynamic t Text -> Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t ())
mode2 uHandle t name part = divClass "rowContainer" $ do
  ev <- clickableTr' $ do
   elClass "td" "statusWidgetName" $ participantNameWidget name part
   elClass "td" "statusWidgetFPSAndLatency" $ participantLatency name part
   elClass "td" "statusWidgetFPSAndLatency" $ participantLoad name part
   elClass "td" "statusWidgetFPSAndLatency" $ participantFPS name part
   elClass "td" "statusWidgetFPSAndLatency" $ participantIP name part
  return ev

mode3 ::  MonadWidget t m  => Dynamic t Text -> Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t ())
mode3 uHandle t name part = divClass "rowContainer" $ do
  ev <- clickableTr' $ do
   elClass "td" "statusWidgetName" $ participantNameWidget name part
   elClass "td" "otherInfo" $ text "info placeholder"
  return ev

participantFPSLatencyAndLoad :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantFPSLatencyAndLoad name part = do
  let latency' = fmap (T.pack . show . floor . realToFrac . (*) 1000 . latency) part
  let load' = fmap (showt . mainLoad) part
  let fps' = fmap (showt . animationLoad) part
  dynText $ latency' <> (constDyn "ms ") <> load' <> (constDyn "% ") <> fps' <>(constDyn "FPS")

participantLatency :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantLatency name part = do
  let latency' = fmap (T.pack . show . floor . realToFrac . (*) 1000 . latency) part
  dynText $ latency' <> (constDyn "ms ")

participantLoad :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantLoad name part = do
  let load' = fmap (showt . mainLoad) part
  dynText $ load' <> (constDyn "% ")

participantFPS :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantFPS name part = do
  let fps' = fmap (showt . animationLoad) part
  dynText $ fps' <>(constDyn "FPS")

participantIP :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantIP name part = do
  let ip' = fmap ipAddress part
  dynText ip'

participantStatusWidget :: MonadWidget t m  => Dynamic t Text -> Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
participantStatusWidget thisUserHandle _ part = do
  initialStatus <- status <$> sample (current part)
  updatedStatus <- fmap updated $ holdUniqDyn $ fmap status part -- event issued only when status changes
  let dynBool = compareHandles <$> thisUserHandle <*> part
  let dynAttrs = attrs <$> dynBool
  s <- textInput $ def & textInputConfig_setValue .~ updatedStatus & textInputConfig_initialValue .~ initialStatus & attributes .~ dynAttrs
  let writeStatusToServer = fmap (\x -> WriteStatus x) $ _textInput_input s --msg only sent when they press a key
  return writeStatusToServer

participantNameLocationAndIPWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameLocationAndIPWidget name part = do
  let child = divClass "statusWidgetPopupChild" $ dynText $ constDyn name <> fmap location' part
  let popup = dynText $ fmap browser part <> "; " <> "IP address: " <> fmap ipAddress part
  tooltipForScrollableTable child popup

compareHandles ::  Text -> Participant -> Bool -- -> Dynamic t Text -> Bool
compareHandles uHandle part = uHandle == (name part)
attrs :: Bool -> Map Text Text
attrs b = "class" =: "code-font" <> "style" =: ("pointer-events: " <> pevents b <> bevents b)
  where
    pevents True  = "auto; "
    pevents False = "none; "
    bevents True = "box-shadow: inset 0 0 3px var(--primary-color); "
    bevents False = ""

location' :: Participant -> Text
location' p = f (Estuary.Types.Participant.location p)
  where
    f x | x == "" = x
        | otherwise = "@" <> x

participantNameWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameWidget name part =  text name

participantLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLocationWidget name part = dynText $ fmap Estuary.Types.Participant.location part

participantActivityWidget :: MonadWidget t m => Event t UTCTime -> Text -> Dynamic t Participant -> m ()
participantActivityWidget t name part =  do
   x <- pollParticipantActivity t part
   dynText x

pollParticipantActivity :: MonadWidget t m => Event t UTCTime -> Dynamic t Participant -> m (Dynamic t Text)
pollParticipantActivity e part = do
  now <- liftIO getCurrentTime
  iv <- fmap (\x -> generateActivityMessage x now) $ sample $ current part -- initial v of part
  let x = attachWith generateActivityMessage (current part) e -- :: Event t Text
  holdDyn iv x -- :: event a -> m (Dyn a)

generateActivityMessage :: Participant -> UTCTime -> Text
generateActivityMessage p t = do
  f (diffUTCTime t (lastEdit p))
  where
    f x | x < 60 = "<1m"
        | x < 120 = "<2m"
        | x < 180 = "<3m"
        | x < 240 = "<4m"
        | x < 300 = "<5m"
        | otherwise = "inact."
