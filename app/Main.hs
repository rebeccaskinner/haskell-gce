module Main where

import Text.Printf
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
  withDataset_ cfg $ \dset -> do
    let dsid = dsRef dset
    display $ printf "dataset: %s" dsid
    withTable_ cfg dsid $ \t -> do
      let tinfo = tRef t
      display $ printf "    table: %s " tinfo
    fail "foo"
  where
    display = liftIO . putStrLn
    dsRef = T.unpack . _dsrefDatasetID . _datasetReference
    tRef = T.unpack . _tableReferenceTableID . _tableReference
