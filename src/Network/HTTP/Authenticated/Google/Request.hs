module Network.HTTP.Authenticated.Google.Request where

import Network.HTTP.Authenticated.Google.OAuth2
import Network.HTTP.Authenticated.Google.GoogleHTTP
import Web.JWT
import Control.Monad.Reader
import Control.Monad.IO.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Aeson.Types
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS


data AuthCfg = AuthCfg
  { _authInfo    :: GoogleAuthData
  , _authClaims  :: ClaimsMap
  , _authData    :: GoogleAuthToken
  }

parseAuthCfg :: ClaimsMap -> IO AuthCfg
parseAuthCfg claims =
  let creds = defaultCreds
  in AuthCfg
     <$> creds
     <*> pure claims
     <*> (creds >>= flip bearerToken claims)

-- TODO: We can check to see if the token is expired and only re-auth
-- then if necessary, but since authorization is quick relative to the
-- duration of most calls, it's probably fine to auth for every request
refreshAuth :: AuthCfg -> IO AuthCfg
refreshAuth (AuthCfg authInfo claims _) = do
  newToken <- bearerToken authInfo claims
  return $ AuthCfg authInfo claims newToken

-- TODO: Manually create an instance of Monad that refreshes the token?
newtype Authenticated a = Authenticated {
  runAuthenticated :: ReaderT AuthCfg IO a
  } deriving (MonadIO, Functor, Applicative, Monad, MonadReader AuthCfg)

runAuthenticatedRequest :: AuthCfg -> Authenticated a -> IO a
runAuthenticatedRequest cfg = flip runReaderT cfg . runAuthenticated

runRequestWithClaims :: ClaimsMap -> Authenticated a -> IO a
runRequestWithClaims claims action =
  parseAuthCfg claims >>= (flip runAuthenticatedRequest action)

authHeader :: AuthCfg -> Header
authHeader auth =
  let token = _gglAccessToken . _authData $ auth
  in ("Authorization", "Bearer " <> token)

mkAuthorizeRequest' :: AuthCfg -> Request -> Request
mkAuthorizeRequest' cfg req =
  let headers = requestHeaders req
  in req { requestHeaders = (authHeader cfg) : headers }

mkAuthorizeRequest :: Request -> Authenticated Request
mkAuthorizeRequest = (<$> ask) . flip mkAuthorizeRequest'

authorizedRequest :: Request -> Authenticated (Response BS.ByteString)
authorizedRequest r = do
  mgr <- liftIO $ newManager tlsManagerSettings
  r' <- mkAuthorizeRequest r
  resp <- liftIO $ httpLbs r' mgr
  return $ BL.toStrict <$> resp

-- | simpleRequest runs a basic get or post request with default values.
simpleRequest :: String -> Maybe BS.ByteString -> Authenticated (Response BS.ByteString)
simpleRequest reqPath body = do
  req <- liftIO $ parseRequest reqPath
  authorizedRequest $ case body of
                        Nothing -> req
                        Just b -> req { requestBody = RequestBodyBS b }

simpleGet :: String -> Authenticated (Response BS.ByteString)
simpleGet url =
  simpleRequest ("GET " ++ url) Nothing

simplePost :: String -> BS.ByteString -> Authenticated (Response BS.ByteString)
simplePost url body =
  simpleRequest ("POST " ++ url) (Just body)
