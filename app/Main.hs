module Main where

import Data.List
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
--   dsets <- datasets cfg
  dsets' <- take (10) <$> datasets' cfg

--  display $ printf "strict length: %d" (length dsets)
--  display $ printf "strict nub length: %d" (length $ nub dsets)

  display $ printf "iter length: %d" (length dsets')
  display $ printf "iter nub length: %d" (length $ nub dsets')


--  mapM_ (display . dsRef) dsets
  -- withDataset_ cfg $ \dset -> do
  --   let dsid = dsRef dset
  --   display $ printf "dataset: %s" dsid
  --   withTable_ cfg dsid $ \t -> do
  --     let tinfo = tRef t
  --     display $ printf "    table: %s " tinfo
  where
    display = liftIO . putStrLn
    dsRef = T.unpack . _dsrefDatasetID . _datasetReference
    tRef = T.unpack . _tableReferenceTableID . _tableReference
