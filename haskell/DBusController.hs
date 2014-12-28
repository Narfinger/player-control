{-# LANGUAGE OverloadedStrings #-}
module DBusController ( SongInfo(..)
                      , StatusInfo(..)
                      , getSongInfo
                      , getStatusInfo
                      ) where

import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Data.String
import DBus
import DBus.Client
import Debug.Trace (trace)

data MusicStatus = Playing | Stopped | Paused  deriving (Show)
data SerieviewerStatus = Running | NotRunning deriving (Show)

data SongInfo = SongInfo { title :: String
                         , artist :: String
                         , album :: String
                         } deriving (Show)
data StatusInfo = StatusInfo { statusmusic :: Maybe MusicStatus
                             , statusserie :: Maybe SerieviewerStatus
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

getTrackInfo :: Client -> Int32 -> IO MethodReturn
getTrackInfo client id =
  let o = objectPath_ "/TrackList" in
  let m = memberName_ "GetMetadata" in
  call_ client (methodCall o "org.freedesktop.MediaPlayer" m)
  { methodCallDestination = Just "org.mpris.clementine",
    methodCallBody = [toVariant id]
  }

createMusicStatus :: (Num a, Eq a) => a -> Maybe MusicStatus
createMusicStatus 0 = Just Playing
createMusicStatus 1 = Just Paused
createMusicStatus 2 = Just Stopped
createMusicStatus _ = Nothing

fromMaybeVariant :: (IsVariant a) => a -> Variant -> a
fromMaybeVariant def v =
    let defv = toVariant def in
    fromMaybe def $ fromVariant v

lookupDictionary :: (IsVariant a) => String -> a -> [(Variant, Variant)] -> a
lookupDictionary key def dict =
    let k = toVariant key
        defv = toVariant ("-" ::String)
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
  SongInfo { title = title', artist = artist', album = album' }

extractTrackID :: MethodReturn -> Maybe Int32
extractTrackID method = fromVariant $ head $ methodReturnBody method

extractPlayStatusInfo :: MethodReturn -> Maybe MusicStatus
extractPlayStatusInfo method =
    let v = head $ methodReturnBody method in
    let body = (fromVariant v) :: Maybe (Int32, Int32, Int32, Int32) in
    --trace $ variantType v
    createMusicStatus $ fromMaybeVariant (0::Int32) $ fst body

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
