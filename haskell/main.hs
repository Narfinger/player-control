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
import Happstack.Server (asContentType, nullConf, serveFile, simpleHTTP
                        , ServerPart, toResponse, ok, Response, dir, seeOther)
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import DBusController (StatusInfo(..), SongInfo(..), getSongInfo, getStatusInfo)
import DBus.Client (connectSession, Client)

data Button = Button { keyword :: String
                       , displayname :: String
                       -- , function ::
                       } deriving (Show)
type Buttons = [Button]

musicbuttons = [Button { keyword = "prev", displayname = "Previous" }
               ,Button { keyword = "next", displayname = "Next"}
               ,Button { keyword = "play", displayname = "Play"}
               ,Button { keyword = "pause", displayname = "Pause"}
               ,Button { keyword = "pp", displayname = "PlayPause"}
               ,Button { keyword = "stop", displayname = "Stop"}
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
indexTemplate song serie =
  let buttonlist = map (\x -> buttonTemplate x) musicbuttons in
  H.div ! A.class_ "wrapperdiv" $ do
    H.div ! A.class_ "musicdiv" $ do
         H.table ! A.class_ "music" $ do
           H.tr $ do
             H.th $ do "Artist"
             H.th $ do "Title"
             H.th $ do "Album"
           H.tr $ do
             H.td $ do H.toHtml $ title $ song
             H.td $ do H.toHtml $ title $ song
             H.td $ do H.toHtml $ title $ song
         H.table  $ do 
           H.tr $ forM_ buttonlist (H.td)

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
main = do {
  client <- DBus.Client.connectSession;
  simpleHTTP nullConf $ msum
       [ dir "style.css" $ serveFile (asContentType "text/css") "../style.css"
       , dir "execute" $ executePage client
       , indexPage client
       ]
  }
