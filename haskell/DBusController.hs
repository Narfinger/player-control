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
import Control.Monad (forever,liftM)
import Control.Monad.Trans  (lift, liftIO)
import Control.Concurrent (threadDelay)
import Debug.Trace (trace)



data PlayStatus = Playing | Stopped | Paused  deriving (Show)
data SerieviewerStatus = Running | NotRunning deriving (Show)

data SongInfo = SongInfo { title :: String
                         , artist :: String
                         , album :: String
                         } deriving (Show)
data StatusInfo = StatusInfo { statusmusic :: Maybe PlayStatus
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

createPlayStatus :: Int -> PlayStatus
createPlayStatus 0 = Playing
createPlayStatus 1 = Paused
createPlayStatus 2 = Stopped

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
                         
extractTrackID :: MethodReturn -> Maybe Int32
extractTrackID method = fromVariant $ head $ methodReturnBody method

extractTrackInfo :: MethodReturn -> SongInfo
extractTrackInfo method =
  let v =  head $ methodReturnBody method  in 
  let Just body =  fromVariant v :: Maybe Dictionary
      dict = dictionaryItems body 
      title'  = lookupDictionary "title"  "-" dict 
      artist' = lookupDictionary "artist" "-" dict
      album'  = lookupDictionary "album"  "-" dict in
  SongInfo { title = title', artist = artist', album = album' }
  
getSongInfo :: Client -> IO SongInfo
getSongInfo client = do
  let methodreturn = callTrack client "GetCurrentTrack"
  Just id <- fmap extractTrackID methodreturn;
  tinfo <- getTrackInfo client id;
  return (extractTrackInfo tinfo)

-- getMusicStatusInfo :: Client -> IO PlayStatus
-- getMusicStatusInfo client = do
  

getStatusInfo :: Client -> IO StatusInfo
getStatusInfo client = do
  return StatusInfo { statusserie = Nothing, statusmusic = Nothing }
