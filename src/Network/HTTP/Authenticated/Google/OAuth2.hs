module Network.HTTP.Authenticated.Google.OAuth2 where

import Text.Printf (printf)
import System.Environment
import Web.JWT
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text.Encoding
import Crypto.PubKey.RSA.Types
import Google.Credentials.ServiceAccount

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

{- | This module implements oauth2 for connecting to google services
   (specifically bigquery) using oauth2.
   See
   https://developers.google.com/identity/protocols/OAuth2ServiceAccount
   for documentation on using oauth2 with a service account.
-}

getClaimSet :: ServiceAccountInfo -> UTCTime -> ClaimsMap -> JWTClaimsSet
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

getClaimSetIO :: ServiceAccountInfo -> ClaimsMap -> IO JWTClaimsSet
getClaimSetIO authData scopes = do
  now <- getCurrentTime
  return $ getClaimSet authData now scopes

defaultClaimSet :: IO JWTClaimsSet
defaultClaimSet = unsafeDefaultCredentials >>= (flip getClaimSetIO bqScopes)

googlePrivateKey :: ServiceAccountInfo -> Maybe PrivateKey
googlePrivateKey = readRsaSecret . encodeUtf8 . _googleAuthPrivateKey

googleSigner :: ServiceAccountInfo -> Maybe Signer
googleSigner = (RSAPrivateKey <$>) . googlePrivateKey

googleToken :: ServiceAccountInfo -> ClaimsMap -> UTCTime -> Maybe JSON
googleToken auth scope now =
  let claims = getClaimSet auth now scope
  in (`encodeSigned` claims) <$> googleSigner auth

googleTokenIO :: ServiceAccountInfo -> ClaimsMap -> IO JSON
googleTokenIO auth scopes = do
  claims <- getClaimSetIO auth scopes
  case googleSigner auth of
    Nothing -> ioError $ userError "unable to get signer"
    Just s -> return $ encodeSigned s claims

defaultGoogleToken :: IO JSON
defaultGoogleToken =
  unsafeDefaultCredentials >>= (flip googleTokenIO bqScopes)

-- | The claims map required for interacting with bigquery.  See
-- https://developers.google.com/identity/protocols/googlescopes#bigqueryv2
bqScopes :: ClaimsMap
bqScopes =
  ClaimsMap $ Map.fromList [("scope", String allScopes)]
  where
    allScopes = T.pack $
      unwords [ "https://www.googleapis.com/auth/bigquery"
              , "https://www.googleapis.com/auth/bigquery.insertdata"]
