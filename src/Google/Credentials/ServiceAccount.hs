{-# LANGUAGE TemplateHaskell #-}
module Google.Credentials.ServiceAccount where

import Data.Aeson (FromJSON (..), Value (..), (.:), eitherDecode)
import Data.Aeson.Types (typeMismatch)
import System.Environment
import Control.Lens.TH
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

envCredentialKey :: String
envCredentialKey = "GOOGLE_APPLICATION_CREDENTIALS"

data ServiceAccountInfo = ServiceAccountInfo
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

makeClassy ''ServiceAccountInfo

instance FromJSON ServiceAccountInfo where
  parseJSON (Object v) = ServiceAccountInfo
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
  parseJSON invalid = typeMismatch "serviceaccount" invalid

type Result a = Either String a

defaultCredentials :: IO (Result ServiceAccountInfo)
defaultCredentials =
  lookupEnv envCredentialKey >>= \case
    Nothing -> return . Left $ envCredentialKey ++ " not set"
    Just path -> credentialsFile path

credentialsFile :: FilePath -> IO (Result ServiceAccountInfo)
credentialsFile path =
  eitherDecode <$> BL.readFile path

unsafeResult :: Result a -> IO a
unsafeResult = \case
  Left err -> ioError $ userError err
  Right a -> return a

defaultProjectID :: IO (Result T.Text)
defaultProjectID =
  (_googleAuthProjectID <$>) <$> defaultCredentials

unsafeDefaultCredentials :: IO ServiceAccountInfo
unsafeDefaultCredentials =
  defaultCredentials >>= unsafeResult
