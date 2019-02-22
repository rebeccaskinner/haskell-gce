module Network.HTTP.Authenticated.Google.GoogleHTTP where

import Network.HTTP.Authenticated.Google.OAuth2
import Web.JWT
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data TokenType = BearerToken deriving (Eq, Show)

data GoogleAuthToken = GoogleAuthToken
  { _gglAccessToken     :: BS.ByteString
  , _gglTokenExpiration :: UTCTime
  , _gglTokenType       :: TokenType
  }

instance Show GoogleAuthToken where
  show (GoogleAuthToken _ expiration t) =
    "Access Token: <redacted>\nExpiration: "
    ++ (show expiration) ++ "\nType: "
    ++ (show t)

data TokenResponse = TokenResponse
  { _respAccessToken     :: T.Text
  , _respTokenExpiration :: Integer
  , _respTokenType       :: TokenType
  }

instance FromJSON TokenResponse where
  parseJSON (Object v) = TokenResponse
    <$> v .: "access_token"
    <*> v .: "expires_in"
    <*> (v .: "token_type" >>= parseTokenType)
    where
      parseTokenType (String "Bearer") = return BearerToken
      parseTokenType invalid = typeMismatch "token_type" invalid
  parseJSON invalid = typeMismatch "token" invalid

mkAuthToken :: TokenResponse -> IO GoogleAuthToken
mkAuthToken (TokenResponse accessToken expiresIn tokenType) = do
  expiration <- calculateExpiration expiresIn <$> getCurrentTime
  return $ GoogleAuthToken (encodeUtf8 accessToken) expiration tokenType
  where
    calculateExpiration :: Integer -> UTCTime -> UTCTime
    calculateExpiration seconds t =
      let seconds' = fromRational @NominalDiffTime . toRational $ secondsToDiffTime seconds
      in addUTCTime seconds' t

bearerToken :: GoogleAuthData -> ClaimsMap -> IO GoogleAuthToken
bearerToken auth scope = do
  manager <- newManager tlsManagerSettings
  tok <- encodeUtf8 <$> googleTokenIO auth scope
  initReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let req = initReq { method = "POST"
                    , host = "www.googleapis.com"
                    , path = "oauth2/v4/token"
                    , port = 443
                    , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                    }
  let req' = urlEncodedBody [("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer")
                            ,("assertion", tok)
                            ] req
  resp <- httpLbs req' manager
  case eitherDecode (responseBody resp) of
    Left err -> ioError $ userError err
    Right token -> mkAuthToken token


defaultBearerToken :: IO GoogleAuthToken
defaultBearerToken =
  defaultCreds >>= (`bearerToken` bqScopes)
