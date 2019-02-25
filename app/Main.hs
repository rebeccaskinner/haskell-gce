module Main where

import Control.Monad
import Control.Monad.IO.Class
import Database.Google.BigQuery
import Database.Google.BigQuery.Types
import Network.HTTP.Authenticated.Google.Request
import Network.HTTP.Authenticated.Google.GoogleHTTP
import Network.HTTP.Authenticated.Google.OAuth2

import qualified Data.Text as T

main :: IO ()
main = runBigQuery $ do
  cfg <- defaultBigQueryConfig
  myDatasets <- (map dsRef . take 10) <$> datasets cfg
  forM_ myDatasets $ \d -> do
    display $ show d
  let dsName = last $ myDatasets
  myTables <- map tRef <$> tables cfg dsName
  forM_ myTables $ \t -> do
    display $ show t
  where
    display = liftIO . putStrLn
    dsRef = T.unpack . _dsrefDatasetID . _datasetReference
    tRef = ("    "++) . T.unpack . _tableReferenceTableID . _tableReference
