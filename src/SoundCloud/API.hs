{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module SoundCloud.API where

import Prelude

import Control.Applicative ((<|>))
import Control.Exception (try, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)

import Data.Aeson (Value, Object, object, ToJSON, FromJSON, genericParseJSON, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isLower, toLower)
import Data.Default (def)
import Data.List (span, uncons)
import Data.List.NonEmpty (NonEmpty(..), fromList, toList, nonEmpty)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, toUpper, unpack)
import Data.Text.Encoding (encodeUtf8)

import Debug.Trace (trace)

import GHC.Generics (Generic)

import Network.HTTP.Req

import System.Directory (doesFileExist)
import System.IO.Error (userError)

import Web.Internal.HttpApiData (ToHttpApiData(..))

scApiUrlBase :: Text
scApiUrlBase = "api.soundcloud.com"

type CredsFilePath = FilePath

newtype ClientId = ClientId [Char]
  deriving stock (Generic)
  deriving newtype (Show, Read, ToHttpApiData)
  deriving anyclass (FromJSON)

newtype ClientSecret = ClientSecret [Char]
  deriving stock (Generic)
  deriving newtype (Show, Read, ToHttpApiData)
  deriving anyclass (FromJSON)

data SoundCloudEnv
  = SoundCloudEnv
  { scClientId :: ClientId
  , scClientSecret :: ClientSecret
  } deriving (Show, Generic, FromJSON)

loadSoundCloudEnv :: CredsFilePath -> IO SoundCloudEnv
loadSoundCloudEnv fp = do
  fileExists <- doesFileExist fp
  when (not fileExists) $
    print ("Credentials file does not exist: " ++ fp)
  lbs <- BSL.readFile fp
  case Aeson.eitherDecode lbs of
    Left err -> throwIO (userError $ "Failed to decode Credentials file: " ++ err)
    Right scEnv -> pure scEnv

newtype SoundCloudM a
  = SoundCloudM { unSoundCloudM :: ReaderT SoundCloudEnv IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader SoundCloudEnv, MonadIO)

runSoundCloudM :: CredsFilePath -> SoundCloudM a -> IO a
runSoundCloudM fp scm = do
  scEnv <- loadSoundCloudEnv fp
  runReaderT (unSoundCloudM scm) scEnv

newtype PlaylistId = PlaylistId Int
  deriving stock (Generic)
  deriving newtype (Show, Read, ToHttpApiData)
  deriving anyclass (FromJSON)

data Endpoint
  = Playlists PlaylistId
  | Resolve Text -- ^ Once we resolve a Url, we can't get it back!

newtype TrackId = TrackId Int
  deriving stock (Generic)
  deriving newtype (Show, Read, ToHttpApiData, ToJSON, FromJSON)

data Track
  = Track
  { trackId :: TrackId
  , trackTitle :: Text
  } deriving stock (Show, Read, Generic)

instance HasRecordFieldsPrefix Track where
  recordFieldsPrefix _ = "track"

instance FromJSON Track where
  parseJSON = genericParseJSON fromJsonDropPrefixCamelToSnake

instance ToJSON Track where
  toJSON =
    Aeson.genericToJSON $
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier =
            unpack . addRecordFieldPrefix (Proxy :: Proxy Track) . pack
        }

data Playlist
  = Playlist
  { playlistId :: PlaylistId
  , playlistTitle :: Text
  , playlistTracks :: [Track]
  } deriving stock (Show, Read, Generic)

instance FromJSON Playlist where
  parseJSON = genericParseJSON fromJsonDropPrefixCamelToSnake

newtype Likes
  = Likes
  { likesTracks :: [Track]
  } deriving stock (Show, Read, Generic)
    deriving newtype (FromJSON)

class HasTracks a where
  getTracks :: a -> [Track]

data ObjectWithTracks
  = PlaylistObject Playlist
  | LikesObject Likes
  deriving (Show, Read, Generic)

getObjectTracks :: ObjectWithTracks -> [Track]
getObjectTracks = \case
  PlaylistObject playlist -> playlistTracks playlist
  LikesObject likes -> likesTracks likes

data ResolvedTracks
  = ResolvedTracks [Track]
  deriving (Show, Read)

instance FromJSON ResolvedTracks where
  parseJSON obj = do
    tracks <-
      fmap getObjectTracks $
        (<|>) (PlaylistObject <$> Aeson.parseJSON obj)
              (LikesObject <$> Aeson.parseJSON obj)
    pure (ResolvedTracks tracks)

-- | Given a camel case string, remove the first lowercase string prefix, and
-- turn the first uppercase letter (hump) into a lowercase letter.
--
-- >>> rmCamelPrefix "thisIsCamelCase" == "isCamelCase"
-- >>> rmCamelPrefix "this_is_not_defined" == "_is_not_defined"
rmCamelPrefix :: [Char] -> [Char]
rmCamelPrefix str =
  case span isLower str of
    (_,[]) -> str
    (_,s:rest) -> toLower s : rest

-- | Given a camel case string, remove the first lowercase string prefix and
-- turn the remainder of the 'humps' into underscores.
--
-- >>> fromJsonDropPrefixToSnakeCase "thisExampleCrazy" == "example_crazy"
-- >>> fromJsonDropPrefixToSnakeCase "this_example_undefined" == "_example_undefined"
fromJsonDropPrefixCamelToSnake :: Aeson.Options
fromJsonDropPrefixCamelToSnake =
    Aeson.defaultOptions { Aeson.fieldLabelModifier = camelToSnake . rmCamelPrefix }
  where
    camelToSnake xs =
      case uncons xs of
        Nothing -> []
        Just y ->
          let (front, back) = span isLower xs in
          case back of
            [] -> front
            (z:zs) -> front ++ ('_' : toLower z : camelToSnake zs)

endpointToUrl :: Endpoint -> (Url 'Https, Option 'Https)
endpointToUrl endpoint =
    (appendUrlPieces (https scApiUrlBase) (toList urlPieces), urlOpts)
  where
    appendUrlPieces url [] = url
    appendUrlPieces url (p:ps) =
      appendUrlPieces (url /: p) ps

    -- NonEmpty is used to check ourselves that _some_ url pieces are returned
    -- in the first list: the url pieces list must not be empty.
    (urlPieces, urlOpts) =
      case endpoint of
        Playlists pid -> ("playlist" :| [showText pid], mempty)
        Resolve rUrl -> ("resolve" :| [], "url" =: rUrl)

endpointToHttpConfig :: Endpoint -> HttpConfig
endpointToHttpConfig = \case
  Playlists _ -> def
  Resolve _ -> def { httpConfigRedirectCount = 0 }

scReq
  :: ( HttpMethod method, HttpBody body, HttpResponse response
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => method
  -> Endpoint
  -> body
  -> Proxy response
  -> SoundCloudM response
scReq method endpoint reqBody proxyRespBody = do
  clientId <- asks scClientId
  let (endpointUrl, endpointOpts) = endpointToUrl endpoint
  runReq def $
    req method endpointUrl reqBody proxyRespBody (endpointOpts <> "client_id" =: clientId )

scGetJSON :: FromJSON a => Endpoint -> SoundCloudM a
scGetJSON endpoint = do
  clientId <- asks scClientId
  responseBody <$>
    scReq GET
      endpoint
      NoReqBody
      jsonResponse

scGetPlaylist :: PlaylistId -> SoundCloudM Playlist
scGetPlaylist = scGetJSON . Playlists

-- | Resolve the api endpoint url of a soundcloud object using it's user
-- displayed url.
--
-- Note: You must specify the type of data you expect to get back, if the type
-- is inferred incorrectly by GHC, at runtime the JSON will fail to parse.
scGetResolve :: Text -> SoundCloudM ResolvedTracks
scGetResolve = scGetJSON . Resolve

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

class HasRecordFieldsPrefix a where
  recordFieldsPrefix :: Proxy a -> Text

addRecordFieldPrefix :: HasRecordFieldsPrefix a => Proxy a -> Text -> Text
addRecordFieldPrefix p suffix =
  recordFieldsPrefix p <> toUpper suffix

showText :: Show a => a -> Text
showText = pack . show
