{-# LANGUAGE OverloadedStrings #-}
module DBusController ( SongInfo(..)
                      , StatusInfo(..)
                      , getSongInfo
                      , getStatusInfo
                      ) where

import DBus
import DBus.Client
import Control.Monad (forever)
import Control.Concurrent (threadDelay)


data SongInfo = SongInfo { title :: String
                         , artist :: String
                         , album :: String
                         } deriving (Show)
data StatusInfo = StatusInfo { statusm :: String
                             , statuss :: String
                             } deriving (Show)



callMedia :: Client -> String -> String -> IO MethodReturn
callMedia client path method =
   call_ client (methodCall (objectPath_ path) "org.freedesktop.MediaPlayer" (memberName_ method))
        { methodCallDestination = Just "org.mpris.clementine"
        }
  
callPlayer :: Client -> String -> IO MethodReturn
callPlayer client method = callMedia client "/Player" method

callTrack :: Client -> String -> IO MethodReturn
callTrack client method = callMedia client "/TrackList" method

getSongInfo :: Client -> IO SongInfo
getSongInfo client = do {
  method <- callTrack client "GetCurrentTrack";

  return SongInfo {title = "Testtitle", artist = "testartist", album = "testalbum"}
  }

getStatusInfo :: Client -> IO StatusInfo
getStatusInfo client = do {
  method <- callPlayer client "GetStatus";

  return StatusInfo { statuss = "tmp", statusm = "tmp" }
  }
