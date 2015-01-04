{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Applicative ((<$>), optional)
import Control.Monad (msum, forM_, when)
import Control.Monad.Trans  (liftIO, lift)
import Data.ByteString.Lazy (ByteString)
import Data.Data
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Typeable
import Happstack.Server (asContentType, nullConf, serveFile, simpleHTTP, simpleHTTPWithSocket
                        , ServerPart, toResponse, ok, Response, dir, seeOther, bindIPv4, port
                        , look)
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import DBusController (StatusInfo(..), SongInfo(..), statusMusicMaybe, getSongInfo, getStatusInfo
                      , playerStop, playerPlay, playerPause, playerPlayPause, playerPrev
                      , playerNext, serieStop)
import DBus.Client (connectSession, Client)

data Button = Button { displayname :: String
                       , function :: Client -> IO ()
                       } deriving (Show)
type Buttons = [(String, Button)]

musicbuttons = [("m_prev",   Button {displayname = "Previous",  function = playerPrev})
                ,("m_next",  Button {displayname = "Next",      function = playerNext})
                ,("m_play",  Button {displayname = "Play",      function = playerPlay})
                ,("m_pause", Button {displayname = "Pause",     function = playerPause})
                ,("m_pp",    Button {displayname = "PlayPause", function = playerPlayPause})
                ,("m_stop",  Button {displayname = "Stop",      function = playerStop})
               ]
seriebuttons = [("s_stop", Button{displayname = "Stop", function = serieStop})
--                -- ,Button { keyword = "s_kill", displayname = "Kill"}
                ]

buttons = musicbuttons ++ seriebuttons

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

buttonTemplate :: (String,Button) -> H.Html
buttonTemplate (value, button) =
  let name = displayname button in
  let v = H.toValue value in 
  H.form ! A.action "/execute" ! A.method "get" $ do
    H.button ! A.type_ "submit" ! A.name "what" ! A.value v $ do
      H.toHtml name

indexTemplate :: SongInfo -> StatusInfo -> H.Html
indexTemplate song statusinfo =
  let buttonlistMusic = map (\x -> buttonTemplate x) musicbuttons
      buttonlistSerie = map (\x -> buttonTemplate x) seriebuttons in
  H.div ! A.class_ "wrapperdiv" $ do
    H.div ! A.class_ "musicdiv" $ do
       H.h2 $ do "Music"
       H.table ! A.class_ "music" $ do
         H.tr $ do
           H.th $ do "Artist"
           H.th $ do "Title"
           H.th $ do "Album"
         H.tr $ do
           H.td $ do H.toHtml $ title $ song
           H.td $ do H.toHtml $ artist $ song
           H.td $ do H.toHtml $ album $ song
       H.table  $ do 
         H.tr $ forM_ buttonlistMusic (H.td)
       H.img ! A.src "/cover" ! A.width "300px" ! A.height "300px"
       H.p ! A.class_ "status" $ do H.toHtml $ "Status: " ++ (show $ statusMusicMaybe statusinfo)
    H.div ! A.class_ "seriediv" $ do
      H.h2 $ do "Serieviewer"
      H.p ! A.class_ "status" $ do  H.toHtml $ "Status: " ++ (show $ statusserie statusinfo)
      H.table $ do
           H.tr $ forM_ buttonlistSerie (H.td)
      H.h2 $ do "Names of Episodes or series or something?"
                                       
appTemplate :: String -> H.Html -> H.Html
appTemplate title body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
      H.body $ do
        body

indexPage :: Client -> ServerPart Response
indexPage client = do {
  status <- liftIO $ getStatusInfo client;
  song <- liftIO $ getSongInfo client;
  ok $ toResponse $ bodyTemplate $ (indexTemplate song status)
  }

getFun :: String -> Client -> IO ()
getFun key =
  let mbutton = lookup key buttons in
  if isNothing mbutton then
    \c -> return ()
  else
    let button = fromJust mbutton in
    function button

executePage :: Client -> ServerPart Response
executePage client = do
  what <- look "what";
  ret <- liftIO $ getFun what client;
  seeOther ("/"::String) (toResponse ("" ::String))

coverPage :: Client -> ServerPart Response
coverPage client = do
  ok $ toResponse $ H.toHtml ("not yet implemented"::String)

main :: IO ()
main = do
  let conf = nullConf
      addr = "127.0.0.1"
  putStrLn "Starting server";
  s <- bindIPv4 addr (port conf); 
  client <- DBus.Client.connectSession;    
  simpleHTTPWithSocket s conf $ msum
       [ dir "style.css" $ serveFile (asContentType "text/css") "../style.css"
       , dir "cover" $ coverPage client
       , dir "execute" $ executePage client
       , indexPage client
       ]
