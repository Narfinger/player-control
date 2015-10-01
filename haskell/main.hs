{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (msum, forM_)
import Control.Monad.Trans  (liftIO)
import Data.Maybe (fromJust, isNothing)
import Happstack.Server (Browsing (DisableBrowsing))
import Happstack.Server (asContentType, nullConf, serveFile, simpleHTTPWithSocket
                        , ServerPart, toResponse, ok, Response, dir, seeOther, bindIPv4, port
                        , look, serveDirectory)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Environment (lookupEnv)
import System.Directory
import MusicController (MusicStatus(..), SongInfo(..),  statusMusicMaybe, getSongInfo, getMusicStatus
                      , playerStop, playerPlay, playerPause, playerPlayPause, playerPrev
                      , playerNext)
import SerieController (SerieviewerStatus(..), getSerieviewerStatus, serieKill, serieNext, serieKillAndNext, seriePlay
                       , vlcPlay, vlcPause, vlcChapterPrev, vlcChapterNext, getSerieList)

import DBus (Address, parseAddress)
import DBus.Client (connect, Client)

data Button = Button { displayname :: String
                       , function :: Client -> IO ()
                       } deriving (Show)
type Buttons = [(String, Button)]

musicbuttons :: [(String, Button)]
musicbuttons = [("m_prev",  Button {displayname = "Previous",  function = playerPrev})
               ,("m_next",  Button {displayname = "Next",      function = playerNext})
               ,("m_play",  Button {displayname = "Play",      function = playerPlay})
               ,("m_pause", Button {displayname = "Pause",     function = playerPause})
               ,("m_pp",    Button {displayname = "PlayPause", function = playerPlayPause})
               ,("m_stop",  Button {displayname = "Stop",      function = playerStop})
               ]
seriebuttons :: [(String, Button)]
seriebuttons = [("s_kill",  Button {displayname = "Kill Player", function = serieKill})
               ,("s_next",  Button {displayname = "Next", function = serieNext})
               ,("s_kn",    Button {displayname = "Kill and Next", function = serieKillAndNext})
                ]
vlcbuttons :: [(String, Button)]
vlcbuttons   = [("vlc_play",  Button {displayname = "Pause",            function = vlcPause})
               ,("vlc_pause", Button {displayname = "Play",             function = vlcPlay})
--               ,("vlc_cprev", Button {displayname = "Previous Chapter", function = vlcChapterPrev})
--               ,("vlc_cnext", Button {displayname = "Next Chapter",     function = vlcChapterNext})
               ] 
buttons :: [(String, Button)]
buttons = musicbuttons ++ seriebuttons ++ vlcbuttons

bodyTemplate :: H.Html ->H.Html
bodyTemplate body =
  H.html $ do
    H.head $ do
      H.title "Amarok Control HASKELL"
      H.meta ! A.httpEquiv "refresh"
             ! A.content "60"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "style.css"
    H.body $ do
      body

buttonTemplate :: String -> (String,Button) -> H.Html
buttonTemplate page (value, button) =
  let name = displayname button
      v = H.toValue value
      p = H.toValue page in 
  H.form ! A.action p ! A.method "get" $ do
    H.button ! A.type_ "submit" ! A.name "what" ! A.value v $ do
      H.toHtml name

mapButtonToTemplate :: [(String,Button)] -> [H.Html]
mapButtonToTemplate = map (\x -> buttonTemplate "/execute" x)

enumerate :: [a] -> [(Int,a)]
enumerate = zip [1..] 

serieButtonTemplate :: (Int, String) -> H.Html
serieButtonTemplate (number, string) =
  let n = H.toValue number in
   H.tr $ do
     H.td $ do H.toHtml number
     H.td $ do H.toHtml string
     H.td $ do 
       H.button ! A.type_ "submit" ! A.name "episode" ! A.value n $ do
         "Play"
           
mapSerieToHtml :: [String] -> [H.Html]
mapSerieToHtml list = 
  let enum = (enumerate list :: [(Int, String)]) in
  map serieButtonTemplate enum

indexTemplate :: SongInfo -> Maybe MusicStatus -> SerieviewerStatus -> [String] -> H.Html
indexTemplate song musicstatus seriestatus serielist =
  let buttonlistMusic = mapButtonToTemplate musicbuttons
      buttonlistSerie = mapButtonToTemplate seriebuttons
      buttonlistVLC   = mapButtonToTemplate vlcbuttons
      buttonlistSeries = mapSerieToHtml serielist
      coverurl = H.toValue $ "/cover/" ++ arturl song in
  H.div ! A.class_ "wrapperdiv" $ do
    H.div ! A.class_ "musicdiv" $ do
       H.h2 $ do "Music"
       H.table ! A.class_ "music" $ do
         H.tr $ do
           H.th $ do "Artist"
           H.th $ do "Title"
           H.th $ do "Album"
           H.th $ do "Duration"
         H.tr $ do
           H.td $ do H.toHtml $ title $ song
           H.td $ do H.toHtml $ artist $ song
           H.td $ do H.toHtml $ album $ song
           H.td $ do H.toHtml $ duration $ song
       H.table  $ do 
         H.tr $ forM_ buttonlistMusic (H.td)
       H.img ! A.src coverurl ! A.width "300px" ! A.height "300px"
       H.p ! A.class_ "status" $ do H.toHtml $ "Status: " ++ (show $ statusMusicMaybe musicstatus)
    H.div ! A.class_ "seriediv" $ do
      H.h2 $ do "Serieviewer"
      H.p ! A.class_ "status" $ do  H.toHtml $ "Status: " ++ (show $ seriestatus)
      H.table $ do
           H.tr $ forM_ buttonlistSerie (H.td)
      H.h2 $ do "VLC Controls"
                H.table $ do
                  H.tr $ forM_ buttonlistVLC (H.td)
      H.h2 $ do "Play Episode"
                H.table $ do
                  H.form ! A.action "/play" ! A.method "get" $ do
                    forM_ buttonlistSeries (H.td)
                                       
appTemplate :: String -> H.Html -> H.Html
appTemplate t body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml t)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
      H.body $ do
        body

indexPage :: Client -> ServerPart Response
indexPage client = do {
  musicstatus <- liftIO $ getMusicStatus client;
  seriestatus <- liftIO $ getSerieviewerStatus client;
  serielist <- liftIO $ getSerieList client;
  song <- liftIO $ getSongInfo client;
  ok $ toResponse $ bodyTemplate $ (indexTemplate song musicstatus seriestatus serielist)
  }

getButtonFun :: String -> Client -> IO ()
getButtonFun key =
  let mbutton = lookup key buttons in
  if isNothing mbutton then
    \_ -> return ()
  else
    let button = fromJust mbutton in
    function button

executePage :: Client -> ServerPart Response
executePage client = do
  what <- look "what";
  ret <- liftIO $ getButtonFun what client;
  seeOther ("/"::String) (toResponse ("" ::String))

playPage :: Client -> ServerPart Response
playPage client = do
  episodenum <- look "what";
  let id = read episodenum ::Int
  _ <- liftIO $ seriePlay client id;
  seeOther ("/bklubb"::String) (toResponse (""::String))

-- coverPage :: Client -> ServerPart Response
-- coverPage client = do
--   song <- liftIO $ getSongInfo client;
--   serveFile (asContentType "image/jpeg") (arturl song)


extractAddress :: String -> String
extractAddress file =
  let myfilter = head . filter (\x -> (head x /= '#'))
      line = myfilter $ lines file in
  drop 25 line  


-- this might break on different systems
-- for example ArchLinux does not seem to create .dbus while Gentoo does
extractDBusAddressFromFile :: IO (Maybe Address)
extractDBusAddressFromFile = do
  homedir <- getHomeDirectory;
  let path = homedir ++ "/.dbus/session-bus/"
  dir <- getDirectoryContents path;
  let file = path ++ (head dir)
  content <- readFile file;
  let addr = extractAddress content
  putStrLn ("DBus Address: " ++ addr);
  return (parseAddress addr);

getDBusAddress :: IO (Maybe Address)
getDBusAddress = do
  env <- lookupEnv "DBUS_SESSION_BUS_ADDRESS";
  case env of
    Nothing ->   extractDBusAddressFromFile
    Just addr -> return (parseAddress addr);


main :: IO ()
main = do
  let conf = nullConf
      addr = "127.0.0.1"
  putStrLn "Finding dbus address";
  Just dbusaddress <- getDBusAddress;
  putStrLn "Starting server";
  putStrLn "WARNING: WE WILL SHARE THE WHOLE /tmp DIRECOTRY!";
  s <- bindIPv4 addr (port conf); 
  client <- DBus.Client.connect dbusaddress;
  putStrLn "Successfully connected to dbus";
  simpleHTTPWithSocket s conf $ msum
       [ dir "style.css" $ serveFile (asContentType "text/css") "../style.css"
       , dir "cover" $ serveDirectory DisableBrowsing [] "/tmp"
       , dir "play" $ playPage client
       , dir "execute" $ executePage client
       , indexPage client
       ]
