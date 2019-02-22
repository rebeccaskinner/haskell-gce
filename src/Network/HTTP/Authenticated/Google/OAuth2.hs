module Network.HTTP.Authenticated.Google.OAuth2 where

import Text.Printf (printf)
import System.Environment
import Web.JWT
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text.Encoding
import Crypto.PubKey.RSA.Types
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

import Data.Aeson

{- | This module implements oauth2 for connecting to google services
   (specifically bigquery) using oauth2.
   See
   https://developers.google.com/identity/protocols/OAuth2ServiceAccount
   for documentation on using oauth2 with a service account.
-}

-- | FIXME: Create a custom show instance that omits the private key
-- | info before merging this
data GoogleAuthData = GoogleAuthData
  { _googleAuthType :: T.Text
  , _googleAuthProjectID :: T.Text
  , _googleAuthPrivateKeyID :: T.Text
  , _googleAuthPrivateKey :: T.Text
  , _googleAuthClientEmail :: T.Text
  , _googleAuthClientID :: T.Text
  , _googleAuthURI :: T.Text
  , _googleAuthTokenURI :: T.Text
  , _googleAuthProviderCert :: T.Text
  , _googleAuthCertURL :: T.Text
  } deriving (Eq)

instance Show GoogleAuthData where
  show auth =
    let redacted = const "<redacted>"
        fields = [ ("Auth type",_googleAuthType)
                 , ("Project ID",_googleAuthProjectID)
                 , ("Private Key ID",redacted)
                 , ("Private Key",redacted)
                 , ("Client Email",redacted)
                 , ("Client ID",_googleAuthClientID)
                 , ("Auth URI",_googleAuthURI)
                 , ("Auth Token URI",_googleAuthTokenURI)
                 , ("Provider Cert",_googleAuthProviderCert)
                 , ("Provider Cert URL",_googleAuthCertURL)
                 ]
    in unlines $
       map (\(a,b) -> a ++ ": " ++ (T.unpack $ b auth)) fields


instance FromJSON GoogleAuthData where
  parseJSON (Object v) = GoogleAuthData
    <$> v .: "type"
    <*> v .: "project_id"
    <*> v .: "private_key_id"
    <*> v .: "private_key"
    <*> v .: "client_email"
    <*> v .: "client_id"
    <*> v .: "auth_uri"
    <*> v .: "token_uri"
    <*> v .: "auth_provider_x509_cert_url"
    <*> v .: "client_x509_cert_url"

defaultCredPath :: String
defaultCredPath = "GOOGLE_APPLICATION_CREDENTIALS"

loadCreds :: FilePath -> IO GoogleAuthData
loadCreds cfgPath = do
  contents <- BL.fromStrict <$> BS.readFile cfgPath
  case eitherDecode contents of
    Left err -> ioError $ userError err
    Right creds -> return creds

defaultCreds :: IO GoogleAuthData
defaultCreds = getEnv defaultCredPath >>= loadCreds

getClaimSet :: GoogleAuthData -> UTCTime -> ClaimsMap -> JWTClaimsSet
getClaimSet auth now scopes =
  let
    iat = numericDate (utcTimeToPOSIXSeconds now)
    exp = numericDate . utcTimeToPOSIXSeconds $ addUTCTime oneHour now
    in JWTClaimsSet
       { iss = stringOrURI $ _googleAuthClientEmail auth
       , sub = Nothing
       , aud = Left <$> googleAudVal
       , Web.JWT.exp = exp
       , nbf = Nothing
       , iat = iat
       , jti = Nothing
       , unregisteredClaims = scopes
       }
  where
    googleAudVal = stringOrURI "https://www.googleapis.com/oauth2/v4/token"
    oneHour = 60 * 60 -- 60 seconds * 60 minutes

getClaimSetIO :: GoogleAuthData -> ClaimsMap -> IO JWTClaimsSet
getClaimSetIO authData scopes = do
  now <- getCurrentTime
  return $ getClaimSet authData now scopes

defaultClaimSet :: IO JWTClaimsSet
defaultClaimSet = defaultCreds >>= (flip getClaimSetIO bqScopes)

googlePrivateKey :: GoogleAuthData -> Maybe PrivateKey
googlePrivateKey = readRsaSecret . encodeUtf8 . _googleAuthPrivateKey

googleSigner :: GoogleAuthData -> Maybe Signer
googleSigner = (RSAPrivateKey <$>) . googlePrivateKey

googleToken :: GoogleAuthData -> ClaimsMap -> UTCTime -> Maybe JSON
googleToken auth scope now =
  let claims = getClaimSet auth now scope
  in (`encodeSigned` claims) <$> googleSigner auth

googleTokenIO :: GoogleAuthData -> ClaimsMap -> IO JSON
googleTokenIO auth scopes = do
  claims <- getClaimSetIO auth scopes
  case googleSigner auth of
    Nothing -> ioError $ userError "unable to get signer"
    Just s -> return $ encodeSigned s claims

defaultGoogleToken :: IO JSON
defaultGoogleToken =
  defaultCreds >>= (flip googleTokenIO bqScopes)

-- | The claims map required for interacting with bigquery.  See
-- https://developers.google.com/identity/protocols/googlescopes#bigqueryv2
bqScopes :: ClaimsMap
bqScopes =
  ClaimsMap $ Map.fromList [("scope", String allScopes)]
  where
    allScopes = T.pack $
      unwords [ "https://www.googleapis.com/auth/bigquery"
              , "https://www.googleapis.com/auth/bigquery.insertdata"]
