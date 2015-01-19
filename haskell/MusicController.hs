{-# LANGUAGE OverloadedStrings #-}
module MusicController ( MusicStatus(..)
                       , SongInfo(..)
                       , statusMusicMaybe
                       , getSongInfo
                       , getMusicStatus
                       , playerStop
                       , playerPlay
                       , playerPause
                       , playerPlayPause
                       , playerPrev
                       , playerNext
                       ) where

import Data.Int
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe
import Data.String
import DBus
import DBus.Client
import Debug.Trace (trace)
import System.IO.Error

data MusicStatus = Playing | Stopped | Paused deriving (Show)
data SongInfo = SongInfo { title :: String
                         , artist :: String
                         , album :: String
                         , arturl :: String
                         } deriving (Show) 

statusMusicMaybe :: Maybe MusicStatus -> MusicStatus
statusMusicMaybe (Just    x) = x
statusMusicMaybe Nothing   = Stopped

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
createMusicStatus _         = Nothing

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
      album'  = lookupDictionary "album"  "-" dict
      arturl' = lookupDictionary "arturl" "?" dict
      artfile = last $ splitOn "/" arturl' in
  SongInfo { title = title', artist = artist', album = album', arturl = artfile  }

extractTrackID :: MethodReturn -> Maybe Int32
extractTrackID method = fromVariant $ head $ methodReturnBody method

extractPlayStatusInfo :: MethodReturn -> Maybe MusicStatus
extractPlayStatusInfo method =
    let v = head $ methodReturnBody method in
    let body = (fromVariant v) :: Maybe (Int32, Int32, Int32, Int32) in
    createMusicStatus $ fromMaybe (0,0,0,0) body

getSongInfo :: Client -> IO SongInfo
getSongInfo client = do
  tinfo <- callPlayer client "GetMetadata";
  return (extractTrackInfo tinfo)

getMusicStatus :: Client -> IO (Maybe MusicStatus)
getMusicStatus client = do
  let m_methodreturn = callPlayer client "GetStatus"
  status <- fmap extractPlayStatusInfo m_methodreturn;
  return status


-- controlling calls
playerStop :: Client -> IO ()
playerStop client = do callPlayer client "Stop"; return ();

playerPlay :: Client -> IO ()
playerPlay client = do callPlayer client "Play"; return ();

playerPause :: Client -> IO ()
playerPause client = do callPlayer client "Pause"; return ();                                 

playerPlayPause :: Client -> IO ()
playerPlayPause client = do
  status <-  getMusicStatus client;
  case status of
    Just Paused  -> playerPause client
    Just Stopped -> playerPlay  client
    Just Playing -> playerPause client
    Nothing      -> return ()
  return ();
               
playerPrev :: Client -> IO ()
playerPrev client = do callPlayer client "Prev"; return ();

playerNext :: Client -> IO ()
playerNext client = do callPlayer client "Next"; return ();
