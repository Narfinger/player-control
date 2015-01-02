{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Applicative ((<$>), optional)
import Control.Monad (msum, forM_)
import Control.Monad.Trans  (liftIO, lift)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Data.Data
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server (asContentType, nullConf, serveFile, simpleHTTP, simpleHTTPWithSocket
                        , ServerPart, toResponse, ok, Response, dir, seeOther, bindIPv4, port)
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import DBusController (StatusInfo(..), SongInfo(..), statusMusicMaybe, getSongInfo, getStatusInfo)
import DBus.Client (connectSession, Client)

data Button = Button { keyword :: String
                       , displayname :: String
                       -- , function ::
                       } deriving (Show)
type Buttons = [Button]

musicbuttons = [Button { keyword = "m_prev", displayname = "Previous" }
               ,Button { keyword = "m_next", displayname = "Next"}
               ,Button { keyword = "m_play", displayname = "Play"}
               ,Button { keyword = "m_pause", displayname = "Pause"}
               ,Button { keyword = "m_pp", displayname = "PlayPause"}
               ,Button { keyword = "m_stop", displayname = "Stop"}
               ]
seriebuttons = [Button { keyword = "s_stop", displayname = "Stop"}
               ,Button { keyword = "s_kill", displayname = "Kill"}
               ]

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

buttonTemplate :: Button -> H.Html
buttonTemplate button =
  let value = keyword button in
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
  
executePage :: Client -> ServerPart Response
executePage client = do {                       
  ok $ toResponse $ bodyTemplate "blubb"
  }

main :: IO ()
main = do
  let conf = nullConf
      addr = "127.0.0.1"
  s <- bindIPv4 addr (port conf); 
  client <- DBus.Client.connectSession;    
  simpleHTTPWithSocket s conf $ msum
       [ dir "style.css" $ serveFile (asContentType "text/css") "../style.css"
       , dir "execute" $ executePage client
       , indexPage client
       ]
