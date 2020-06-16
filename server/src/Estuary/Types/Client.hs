{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Client where

-- The type Client represents all the information that the Estuary server maintains
-- about each connected client. (The function clientToParticipant converts this record
-- to the related type Participant that is transmitted from the server to all clients as part
-- of the process of sharing information about the members of an ensemble.)

import Network.WebSockets
import Data.Time
import Data.Text
import Control.Concurrent.STM

import Estuary.Types.Participant
import Estuary.Types.EnsembleS

type ClientHandle = Int

data Client = Client {
  handle :: ClientHandle,
  connection :: Connection,
  browserInfo :: Text,
  memberOfEnsemble :: Maybe (TVar EnsembleS),
  handleInEnsemble :: Text,
  locationInEnsemble :: Text,
  statusInEnsemble :: Text,
  lastEditInEnsemble :: UTCTime,
  authenticatedInEnsemble :: Bool,
  clientMainLoad :: Int,
  clientAnimationFPS :: Int,
  clientAnimationLoad :: Int,
  clientLatency :: NominalDiffTime
}

newClient :: UTCTime -> ClientHandle -> Connection -> Client
newClient t h c = Client {
  handle = h,
  connection = c,
  browserInfo = "",
  memberOfEnsemble = Nothing,
  handleInEnsemble = "",
  locationInEnsemble = "",
  statusInEnsemble = "",
  lastEditInEnsemble = t,
  authenticatedInEnsemble = False,
  clientMainLoad = 0,
  clientAnimationFPS = 0,
  clientAnimationLoad = 0,
  clientLatency = 0
}

clientToParticipant :: Client -> Participant
clientToParticipant c = Participant {
  name = handleInEnsemble c,
  location = locationInEnsemble c,
  status = statusInEnsemble c,
  lastEdit = lastEditInEnsemble c,
  mainLoad = clientMainLoad c,
  animationFPS = clientAnimationFPS c,
  animationLoad = clientAnimationLoad c,
  latency = clientLatency c,
  browser = browserInfo c
  }
