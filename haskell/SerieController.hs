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
import DBus.Client (call_, Client, ClientError)
import Control.Concurrent (threadDelay)
import Control.Exception (try, tryJust)
import Control.Monad (guard)
import Data.Maybe (fromMaybe, isNothing)
data SerieviewerStatus = Running | NotRunning deriving (Show)

extractException :: Either ClientError MethodReturn -> Maybe MethodReturn
extractException (Left e)  = Nothing
extractException (Right a) = Just a

callDBus :: Client -> ObjectPath -> InterfaceName -> BusName -> MemberName-> IO (Maybe MethodReturn)
callDBus client objectpath interfacename methoddestination membername = do
  r <- try (call_ client (methodCall objectpath interfacename membername)
                           { methodCallDestination = Just methoddestination
                           })
  return $ extractException r

-- serie calls
callSerie :: Client -> String -> IO (Maybe MethodReturn)
callSerie client method = do
  let m = memberName_ method
  callDBus client "/Serieviewer" "org.serieviewer" "org.serieviewer" m
  
callVLCWithInterface :: Client -> String -> String -> IO (Maybe MethodReturn)
callVLCWithInterface client interfacename membername = do
  let m = memberName_ membername
  let i = interfaceName_ interfacename
  callDBus client "/org/mpris/MediaPlayer2" i "org.mpris.MediaPlayer2.vlc" m

callVLC :: Client -> String -> IO (Maybe MethodReturn)
callVLC client membername =
  callVLCWithInterface client "org.mpris.MediaPlayer2" membername

callVLCPlayer :: Client -> String -> IO (Maybe MethodReturn)
callVLCPlayer client membername =
  callVLCWithInterface client "org.mpris.MediaPlayer2.Player" membername

callDBusNames :: Client -> IO (Maybe MethodReturn)
callDBusNames client =
  let o = objectPath_ "/"
      m = memberName_ "ListNames" in
  callDBus client "/" "org.freedesktop.DBus" "org.freedesktop.DBus" m

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

extractSerieNames :: Maybe MethodReturn -> [String]
extractSerieNames method =
  case method of                -- for some reason matching didn't work, check why
   Nothing -> []
   Just m ->
     let v = head $ methodReturnBody m in
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
  let o = objectPath_ "/Serieviewer"
      m = memberName_ "playIndex"
      i = show id in
   call_ client (methodCall o "org.serieviewer" m)
   {
     methodCallDestination = Just "org.serieviewer",
     methodCallBody = [toVariant i]
   };
   return ()
  
-- VLC Calls
vlcPause :: Client -> IO ()
vlcPause client = do callVLCPlayer client "Pause"; return (); 

vlcPlay :: Client -> IO ()
vlcPlay client = do callVLCPlayer client "Play"; return ();

vlcChapterPrev :: Client -> IO ()
vlcChapterPrev client = do return ()

vlcChapterNext :: Client -> IO ()
vlcChapterNext client = do return ()
