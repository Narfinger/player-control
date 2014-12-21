{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

callPlayer :: Client -> String -> IO MethodReturn
callPlayer client method =
  call_ client (methodCall "/Player" "org.freedesktop.MediaPlayer" (memberName_ method))
        { methodCallDestination = Just "org.mpris.clementine"
        }

-- trying signal stuff but this does not work, so i am deleting it at the moment
-- signalCallback :: Signal -> IO ()
-- signalCallback signal_ = putStrLn $ "sig"--  ++ (fromVariant (signalBody signal))
--                          where
--                            sig :: String
--                            Just sig = fromVariant $ head $ signalBody signal_
--   r <- addMatch client ( matchAny { matchPath = Just "/Player",
--                      matchSender = Just "org.mpris.clementine",
--                      matchInterface = Just  "org.freedesktop.MediaPlayer",
--                      matchMember = Just (memberName_ "TrackChange")
--                    }) signalCallback;


main :: IO ()
main = do {
  -- this way the monad/io stuff contradicts with currying
  client <- connectSession;

  
  fun <- callPlayer client;
  fun "Pause";
  }



  
  -- connectSession >>= \client callPlayer client
  -- client <- connectSession
  -- player <- callPlayer client
  -- player "Pause"

  -- this way i get type missmatch
  -- let client = connectSession in
  --   let player = callPlayer client in
  --   player "Pause"

  -- this was working
  -- reply <- call_ client (methodCall "/Player" "org.freedesktop.MediaPlayer" "Pause")
  -- { methodCallDestination = Just "org.mpris.clementine"
  -- }
