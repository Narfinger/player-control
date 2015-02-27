{-# LANGUAGE OverloadedStrings #-}
module SerieController ( SerieviewerStatus(..)
                       , getSerieviewerStatus
                       , getSerieList
                       , serieKill
                       , serieNext
                       , serieKillAndNext
                       , seriePlay
                       , vlcPause
                       , vlcPlay
                       , vlcChapterPrev
                       , vlcChapterNext
                       ) where

import DBus
import DBus.Client
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
data SerieviewerStatus = Running | NotRunning deriving (Show)

-- serie calls
callSerie :: Client -> String -> IO MethodReturn
callSerie client method =
  let o = objectPath_ "/Serieviewer"
      m = memberName_ method in
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

extractSerieNames :: MethodReturn -> [String]
extractSerieNames method =
  let v = head $ methodReturnBody method in
  fromMaybe [] $ fromVariant v

getSerieList :: Client -> IO [String]
getSerieList client = do
  let m_methodreturn = callSerie client "getSerieNameList"
  list <- fmap extractSerieNames m_methodreturn;
  return list

serieKill :: Client -> IO ()
serieKill client = do callVLC client "Quit"; return ();

serieNext :: Client -> IO ()
serieNext client = do callSerie client "playNextInSerie"; return ();

serieKillAndNext :: Client -> IO ()
serieKillAndNext client = do
  serieKill client;
  threadDelay 5;
  serieNext client;

seriePlay :: Client -> Int -> IO ()
seriePlay client id = do
  return ()
  -- let o = objectPath_ "/Serieviewer"
  --     m = memberName_ method in
  --  call_ client (methodCall o "org.serieviewer" m)
  --   { methodCallDestination = Just "org.serieviewer"
  --   }
  --  return ()

  -- let o = objectPath_ "/Serieviewer"
  --     m = memberName_ "playIndex" in
  -- call_ client (methodCall o "org.serieviewer" m)
  -- { methodCallDestination = Just "org.serieviewer",
  --   methodCallBody = [toVariant id]
  -- }

-- VLC Calls
vlcPause :: Client -> IO ()
vlcPause client = do callVLCPlayer client "Pause"; return (); 

vlcPlay :: Client -> IO ()
vlcPlay client = do callVLCPlayer client "Play"; return ();

vlcChapterPrev :: Client -> IO ()
vlcChapterPrev client = do return ()

vlcChapterNext :: Client -> IO ()
vlcChapterNext client = do return ()
