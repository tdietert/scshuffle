{-# LANGUAGE ExtendedDefaultRules #-}

module Html
( Html
, renderPage
, indexHtml
, tracksHtml
) where

import Control.Monad (forM_)
import Data.Text.Lazy (Text, toStrict)
import Lucid
import Lucid.Bootstrap
import SoundCloud.API

renderPage :: Html () -> Text
renderPage body = renderText (siteTemplate body)

indexHtml :: Html ()
indexHtml =
  form_ [method_ "post", action_ "/resolve" ] $ do
    input_ [name_ "url", type_ "text", placeholder_ "Enter a url"]
    input_ [name_ "shuffle", type_ "submit", value_ "shuffle"]

tracksHtml :: [Track] -> Html ()
tracksHtml tracks =
  container_ $
    forM_ tracks $ \track ->
      row_ $ do
        div_ [classes_ ["col", "col-lg-6"]] $
          toHtml (trackTitle track)

siteTemplate :: Html () -> Html ()
siteTemplate htmlBody =
  html_ $ do
    head_ $ do
      title_ "scshuffle"
      meta_
        [ name_ "description"
        , content_ "A web app to play your SoundCloud playlists or likes, shuffled!"
        ]
      meta_
        [ name_ "author"
        , content_ "Thomas Dietert"
        ]
      link_
        [ rel_ "stylesheet"
        , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        , integrity_ "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO"
        , crossorigin_ "anonymous"
        ]
      with (script_ "")
        [ src_ "https://code.jquery.com/jquery-3.3.1.slim.min.js"
        , integrity_ "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
        , crossorigin_ "anonymous"
        ]
      with (script_ "")
        [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
        , integrity_ "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
        , crossorigin_ "anonymous"
        ]
      with (script_ "")
        [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
        , integrity_ "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
        , crossorigin_ "anonymous"
        ]

    body_ htmlBody
