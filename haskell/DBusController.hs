{-# LANGUAGE OverloadedStrings #-}
module DBusController ( SongInfo(..)
                      , StatusInfo(..)
                      , getSongInfo
                      , getStatusInfo
                      ) where

import Data.Int
import Data.Map
import Data.Maybe
import DBus
import DBus.Client
import Control.Monad (forever,liftM)
import Control.Monad.Trans  (lift, liftIO)
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

getTrackInfo :: Client -> Int32 -> IO MethodReturn
getTrackInfo client id =
  let o = objectPath_ "/TrackList" in
  let m = memberName_ "GetMetadata" in
  call_ client (methodCall o "org.freedesktop.MediaPlayer" m)
  { methodCallDestination = Just "org.mpris.clementine",
    methodCallBody = [toVariant id]
  }

extractTrackID :: MethodReturn -> Maybe Int32
extractTrackID method = fromVariant $ head $ methodReturnBody method
  
extractTrackInfo :: MethodReturn -> SongInfo
extractTrackInfo method =
  let Just body = fromVariant (methodReturnBody method !! 0) in
  SongInfo { title = show body, artist = "-", album = "t" }
  -- case body of
  --   Nothing -> SongInfo { title = "-", artist = "-", album = "-" }
  --   Just map ->
  --     SongInfo { title = show body, artist = show $ size map, album = "t" } 


getSongInfo :: Client -> IO SongInfo
getSongInfo client = do
  let methodreturn = callTrack client "GetCurrentTrack"
  Just id <- (fmap extractTrackID) methodreturn;
  tinfo <- getTrackInfo client id;
  return (extractTrackInfo tinfo)

getStatusInfo :: Client -> IO StatusInfo
getStatusInfo client = do
  return StatusInfo { statusm = "tmp", statuss = "tmp" }
  
  -- trackreturn <- callTrack client "GetCurrentTrack";
  -- id <- lift currentTrackID trackreturn;
  -- lift currentTrackInfo id;
  
  

-- getStatusInfo :: Client -> IO StatusInfo
-- getStatusInfo client = do {
--   method <- callPlayer client "GetStatus";

--   return StatusInfo { statuss = "tmp", statusm = "tmp" }
--   }


-- do functions and try to lift them?
-- if i have something that is pure and i want to use it for monad types i can use lift
--( $ )    ::                                     (a ->   b) ->   a ->      b
--(<$>)    ::  Functor     f                 =>   (a ->   b) -> f a -> f    b
--(<*>)    ::  Applicative f                 => f (a ->   b) -> f a -> f    b
--(=<<)    ::  Monad       m                 =>   (a -> m b) -> m a -> m    b
--traverse :: (Applicative f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
