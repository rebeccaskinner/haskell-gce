module Database.Google.BigQuery where

import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Authenticated.Google.Request
import Network.HTTP.Authenticated.Google.GoogleHTTP
import Network.HTTP.Authenticated.Google.OAuth2
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Google.Credentials.ServiceAccount
import Text.Printf
import Data.Aeson

import Database.Google.BigQuery.Types

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data BigQueryConfig = BigQueryConfig
  { _bqProjectID :: String }

datasetListURL :: BigQueryConfig -> String
datasetListURL =
  printf "https://www.googleapis.com/bigquery/v2/projects/%s/datasets" . _bqProjectID

tableListURL :: BigQueryConfig -> String -> String
tableListURL (BigQueryConfig projectID) datasetID =
  printf "https://www.googleapis.com/bigquery/v2/projects/%s/datasets/%s/tables" projectID datasetID

fetchPage :: (FromJSON a, NamedKey a) => String -> Authenticated (PaginatedResults a)
fetchPage url = do
    rawResponse <- (BL.fromStrict . responseBody) <$> simpleGet url
    case eitherDecode rawResponse of
      Left err -> fail err
      Right decoded -> return decoded

getItems :: (FromJSON a, NamedKey a) => BigQueryConfig -> String -> Authenticated [a]
getItems bqCfg u =
  getAllPages u (nextRequestString u) fetchPage
  where
    nextRequestString :: String -> String -> String
    nextRequestString = printf "%s?pageToken=%s"

datasets :: BigQueryConfig -> Authenticated [Dataset]
datasets = getItems <*> datasetListURL

tables :: BigQueryConfig -> String -> Authenticated [TableBasicInfo]
tables bqCfg dataset =
  getItems bqCfg $ tableListURL bqCfg dataset

runBigQuery :: Authenticated a -> IO a
runBigQuery = runRequestWithClaims bqScopes

defaultBigQueryConfig :: Authenticated BigQueryConfig
defaultBigQueryConfig = liftIO $ do
  projID <- join (unsafeResult <$> defaultProjectID)
  return . BigQueryConfig . T.unpack $ projID
