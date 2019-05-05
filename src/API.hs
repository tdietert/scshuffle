
module API where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Text.Lazy (Text)
import Html
import SoundCloud.API
  ( SoundCloudM
  , ResolvedTracks(..)
  , runSoundCloudM
  , scGetResolve
  )
import Web.Scotty.Trans
  ( ActionT
  , ScottyT
  , ScottyError
  , scottyT
  , get
  , html
  , param
  , post
  )

runServer :: IO ()
runServer =
  scottyT 3000 (runSoundCloudM "creds.json") server

htmlResponse :: Html () -> ActionT Text SoundCloudM ()
htmlResponse = html . renderPage

server :: ScottyT Text SoundCloudM ()
server = do
  get "/" $
    htmlResponse indexHtml

  -- Accepts form data with a single field "url"
  post "/resolve" $ do
    url <- param "url"
    ResolvedTracks tracks <- lift (scGetResolve url)
    liftIO $ mapM_ print tracks
    htmlResponse (tracksHtml tracks)
