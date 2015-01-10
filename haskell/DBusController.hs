{-# LANGUAGE OverloadedStrings #-}
module DBusController ( SongInfo(..)
                      , StatusInfo(..)
                      , statusMusicMaybe
                      , getSongInfo
                      , getStatusInfo
                      , playerStop
                      , playerPlay
                      , playerPause
                      , playerPlayPause
                      , playerPrev
                      , playerNext
                      , serieKill
                      , serieNext
                      , serieKillAndNext
                      ) where

import Control.Concurrent (threadDelay)
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Data.String
import DBus
import DBus.Client
import Debug.Trace (trace)

data MusicStatus = Playing | Stopped | Paused deriving (Show)
data SerieviewerStatus = Running | NotRunning deriving (Show)

data SongInfo = SongInfo { title :: String
                         , artist :: String
                         , album :: String
                         , arturl :: String
                         } deriving (Show) 

data StatusInfo = StatusInfo { statusmusic :: Maybe MusicStatus
                             , statusserie :: Maybe SerieviewerStatus
                             } deriving (Show)

statusMusicMaybe :: StatusInfo -> MusicStatus
statusMusicMaybe x = fromMaybe Stopped $ statusmusic x 

callMedia :: Client -> String -> String -> IO MethodReturn
callMedia client path method =
   call_ client (methodCall (objectPath_ path) "org.freedesktop.MediaPlayer" (memberName_ method))
        { methodCallDestination = Just "org.mpris.clementine"
        }

callPlayer :: Client -> String -> IO MethodReturn
callPlayer client method = callMedia client "/Player" method

callTrack :: Client -> String -> IO MethodReturn
callTrack client method = callMedia client "/TrackList" method

getTrackInfo :: Client -> Int32 -> IO MethodReturn
getTrackInfo client id =
  let o = objectPath_ "/TrackList" in
  let m = memberName_ "GetMetadata" in
  call_ client (methodCall o "org.freedesktop.MediaPlayer" m)
  { methodCallDestination = Just "org.mpris.clementine",
    methodCallBody = [toVariant id]
  }

--First integer: 0 = Playing, 1 = Paused, 2 = Stopped. 
--Second interger: 0 = Playing linearly , 1 = Playing randomly. 
--Third integer: 0 = Go to the next element once the current has finished playing , 1 = Repeat the current element 
--Fourth integer: 0 = Stop playing once the last element has been played, 1 = Never give up playing
createMusicStatus :: (Num a, Eq a) => (a,a,a,a) -> Maybe MusicStatus
createMusicStatus (0,_,_,_) = Just Playing
createMusicStatus (1,_,_,_) = Just Paused
createMusicStatus (2,_,_,_) = Just Stopped
createMusicStatus _ = Nothing

fromMaybeVariant :: (IsVariant a) => a -> Variant -> a
fromMaybeVariant def v =
    let defv = toVariant def in
    fromMaybe def $ fromVariant v

lookupDictionary :: (IsVariant a) => String -> a -> [(Variant, Variant)] -> a
lookupDictionary key def dict =
    let k = toVariant key
        defv = toVariant ("???" ::String)
        v = fromMaybe defv $ lookup k dict in
    fromMaybeVariant def $ fromMaybeVariant defv v

extractTrackInfo :: MethodReturn -> SongInfo
extractTrackInfo method =
  let v =  head $ methodReturnBody method  in
  let Just body =  fromVariant v :: Maybe Dictionary
      dict = dictionaryItems body
      title'  = lookupDictionary "title"  "-" dict
      artist' = lookupDictionary "artist" "-" dict
      album'  = lookupDictionary "album"  "-" dict in
      -- arturl' = lookupDictionary "arturl" "?" dict in
  SongInfo { title = title', artist = artist', album = album', arturl = show body  }

extractTrackID :: MethodReturn -> Maybe Int32
extractTrackID method = fromVariant $ head $ methodReturnBody method

extractPlayStatusInfo :: MethodReturn -> Maybe MusicStatus
extractPlayStatusInfo method =
    let v = head $ methodReturnBody method in
    let body = (fromVariant v) :: Maybe (Int32, Int32, Int32, Int32) in
    createMusicStatus $ fromMaybe (0,0,0,0) body

getSongInfo :: Client -> IO SongInfo
getSongInfo client = do
  let methodreturn = callTrack client "GetCurrentTrack"
  Just id <- fmap extractTrackID methodreturn;
  tinfo <- getTrackInfo client id;
  return (extractTrackInfo tinfo)

getStatusInfo :: Client -> IO StatusInfo
getStatusInfo client = do
  let methodreturn = callPlayer client "GetStatus"
  statusm' <- fmap extractPlayStatusInfo methodreturn;
  return StatusInfo { statusserie = Nothing, statusmusic = statusm'}


-- controlling calls
playerStop :: Client -> IO ()
playerStop client = do callPlayer client "Stop"; return ();

playerPlay :: Client -> IO ()
playerPlay client = do callPlayer client "Play"; return ();

playerPause :: Client -> IO ()
playerPause client = do callPlayer client "Pause"; return ();                                 

playerPlayPause :: Client -> IO ()
playerPlayPause client = do
  status <- (fmap statusMusicMaybe) $ getStatusInfo client;
  case status of
    Paused  -> callPlayer client "Pause"
    Stopped -> callPlayer client "Play"
    Playing -> callPlayer client "Pause"
  return ();
               
playerPrev :: Client -> IO ()
playerPrev client = do callPlayer client "Prev"; return ();

playerNext :: Client -> IO ()
playerNext client = do callPlayer client "Next"; return ();


-- serie calls
callSerie :: Client -> String -> IO MethodReturn
callSerie client method =
  let o = objectPath_ "/Serieviewer" in
  let m = memberName_ method in
  call_ client (methodCall o "org.serieviewer" m)
  { methodCallDestination = Just "org.serieviewer"
  }

callVLC :: Client -> String -> IO MethodReturn
callVLC client method =
  let o = objectPath_ "/org/mpris/MediaPlayer2" in
  let m = memberName_ method in
  call_ client (methodCall o "org.mpris.MediaPlayer2" m)
  { methodCallDestination = Just "org.mpris.MediaPlayer2.vlc"
  }

serieKill :: Client -> IO ()
serieKill client = do callVLC client "Quit"; return ();

serieNext :: Client -> IO ()
serieNext client = do callSerie client "playNextInSerie"; return ();

serieKillAndNext :: Client -> IO ()
serieKillAndNext client = do
  serieKill client;
  threadDelay 1;
  serieNext client;

