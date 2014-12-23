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



callPlayer :: Client -> String -> IO MethodReturn
callPlayer client method =
  call_ client (methodCall "/Player" "org.freedesktop.MediaPlayer" (memberName_ method))
        { methodCallDestination = Just "org.mpris.clementine"
        }



-- foo :: IO ()
-- foo = do {
--   -- this way the monad/io stuff contradicts with currying
--   client <- connectSession;

  
--   fun <- callPlayer client;
--   fun "Pause";
--   }

getSongInfo :: SongInfo
getSongInfo = SongInfo { title = "t", artist = "T", album = "j" }

getStatusInfo :: StatusInfo
getStatusInfo = StatusInfo { statusm = "playingtest", statuss = "playingtess" }
