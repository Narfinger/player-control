{-# LANGUAGE OverloadedStrings #-}
module SerieController ( SerieviewerStatus(..)
                       , getSerieviewerStatus
                       , serieKill
                       , serieNext
                       , serieKillAndNext
                       , vlcPause
                       , vlcPlay
                       , vlcChapterPrev
                       , vlcChapterNext
                       ) where

import DBus
import DBus.Client
import Control.Concurrent (threadDelay)

data SerieviewerStatus = Running | NotRunning deriving (Show)

-- serie calls
callSerie :: Client -> String -> IO MethodReturn
callSerie client method =
  let o = objectPath_ "/Serieviewer" in
  let m = memberName_ method in
  call_ client (methodCall o "org.serieviewer" m)
  { methodCallDestination = Just "org.serieviewer"
  }

callVLCWithInterface :: Client -> String -> String -> IO MethodReturn
callVLCWithInterface client interfacename membername =
  let o = objectPath_ "/org/mpris/MediaPlayer2"
      m = memberName_ membername
      i = interfaceName_  interfacename in
   call_ client (methodCall o i m)
   { methodCallDestination = Just "org.mpris.MediaPlayer2.vlc"
   }

callVLC :: Client -> String -> IO MethodReturn
callVLC client membername =
  callVLCWithInterface client "org.mpris.MediaPlayer2" membername

callVLCPlayer :: Client -> String -> IO MethodReturn
callVLCPlayer client membername =
  callVLCWithInterface client "org.mpris.MediaPlayer2.Player" membername

callDBusNames :: Client -> IO MethodReturn
callDBusNames client =
  let o = objectPath_ "/"
      m = memberName_ "ListNames" in
  call_ client (methodCall o "org.freedesktop.DBus" m)
  { methodCallDestination = Just "org.freedesktop.DBus"
  }

serieStatus :: MethodReturn -> SerieviewerStatus
serieStatus method =
  let v = head $ methodReturnBody method
      list = (fromVariant v ::Maybe [String])
      test = \l -> "org.serieviewer" `elem` l
      exists = maybe False test list in
  if exists then Running else NotRunning


getSerieviewerStatus :: Client -> IO SerieviewerStatus
getSerieviewerStatus client = do
  let m_methodreturn = callDBusNames client
  status <- fmap serieStatus m_methodreturn;
  return status

serieKill :: Client -> IO ()
serieKill client = do callVLC client "Quit"; return ();

serieNext :: Client -> IO ()
serieNext client = do callSerie client "playNextInSerie"; return ();

serieKillAndNext :: Client -> IO ()
serieKillAndNext client = do
  serieKill client;
  threadDelay 5;
  serieNext client;


-- VLC Calls
vlcPause :: Client -> IO ()
vlcPause client = do callVLCPlayer client "Pause"; return (); 

vlcPlay :: Client -> IO ()
vlcPlay client = do callVLCPlayer client "Play"; return ();

vlcChapterPrev :: Client -> IO ()
vlcChapterPrev client = do return ()

vlcChapterNext :: Client -> IO ()
vlcChapterNext client = do return ()
