{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Google.BigQuery.Types where

import Text.Printf
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Map.Strict
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS


data DatasetReference = DatasetReference
  { _dsrefDatasetID :: T.Text
  , _dsrefProjectID :: T.Text
  } deriving (Eq, Show, Ord)

instance FromJSON DatasetReference where
  parseJSON (Object v) = DatasetReference
    <$> v .: "datasetId"
    <*> v .: "projectId"
  parseJSON invalid = typeMismatch "datasetReference" invalid

data Dataset = Dataset
  { _datasetKind :: T.Text
  , _datasetID :: T.Text
  , _datasetReference :: DatasetReference
  , _datasetLocation :: T.Text
  } deriving (Eq, Show, Ord)

instance FromJSON Dataset where
  parseJSON (Object v) = Dataset
    <$> v .: "kind"
    <*> v .: "id"
    <*> v .: "datasetReference"
    <*> v .: "location"
  parseJSON invalid = typeMismatch "dataset" invalid



class NamedKey a where
  keyName :: T.Text

instance NamedKey Dataset where keyName = "datasets"


data TableReference = TableReference
  { _tableReferenceProjectID :: T.Text
  , _tableReferenceDatasetID :: T.Text
  , _tableReferenceTableID   :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON TableReference where
  parseJSON (Object v) = TableReference
    <$> v .: "projectId"
    <*> v .: "datasetId"
    <*> v .: "tableId"
  parseJSON invalid = typeMismatch "tableReference" invalid

-- | TableBasicInfo contains basic table info extracted from a table
-- | list.  It does not include the full schema, which is only available
-- | by getting a specific table.
data TableBasicInfo = TableBasicInfo
  { _tableKind         :: T.Text
  , _tableID           :: T.Text
  , _tableType         :: T.Text
  , _tableReference    :: TableReference
  , _tableFriendlyName :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON TableBasicInfo where
  parseJSON (Object v) = TableBasicInfo
    <$> v .: "kind"
    <*> v .: "id"
    <*> v .: "type"
    <*> v .: "tableReference"
    <*> v .:? "friendlyName"
  parseJSON invalid = typeMismatch "tables" invalid

instance NamedKey TableBasicInfo where keyName = "tables"

-- | NB: Query parameters are currently unsupported when using legacy
-- | SQL mode.  The public API for generating queries is likely to
-- | change significantly as more features are added.
data Query = Query
  { _queryQuery          :: T.Text
  , _queryRowLimit       :: Maybe Int
  , _queryDefaultDataset :: Maybe DatasetReference
  , _queryTimeout        :: Maybe NominalDiffTime
  , _queryDryRun         :: Bool
  , _queryUseCache       :: Bool
  , _queryUseLegacySQL   :: Bool
  } deriving (Eq, Show)

class Paginated a b | a -> b where
  nextPageID :: a -> Maybe String
  getPage :: a -> [b]

data PaginatedResults a = PaginatedResults
  { _paginatedNextPageToken :: Maybe String
  , _paginatedValues :: [a]
  }

instance (FromJSON a, NamedKey a) => FromJSON (PaginatedResults a) where
    parseJSON (Object v) = PaginatedResults
      <$> v .:? "nextPageToken"
      <*> v .:  keyName @a
    parseJSON invalid = typeMismatch "paginated_results" invalid

instance Paginated (PaginatedResults a) a where
  nextPageID = _paginatedNextPageToken
  getPage = _paginatedValues

getAllPages :: (Monad m, MonadIO m, Paginated a b) => String -> (String -> String) -> (String -> m a) -> m [b]
getAllPages = getAllPages' []
  where
    getAllPages' ::
      (Monad m, MonadIO m, Paginated a b) =>
      [b] -> String -> (String -> String) -> (String -> m a) -> m [b]
    getAllPages' carry s nextS nextF =
      (carry <>) <$> do
      liftIO $ putStrLn "getting page"
      page <- nextF s
      let carry' = getPage page
      case nextPageID page of
        Nothing -> return carry'
        Just nextID -> do
          getAllPages' carry' (nextS nextID) nextS nextF


data PageIter carryType urlType = Done carryType
                                | Iter carryType urlType

withPages'
  :: (Monad m, MonadIO m, Paginated a b) =>
  (String -> String) -> (String -> m a) -> PageIter [m c] String -> (b -> m c) -> m [c]
withPages' _ _ (Done carry) _ = sequence carry
withPages' normalizeURL fetchPage  (Iter carry thisURL) pageAction = do
  liftIO . putStrLn $ printf "getting URL: %s" thisURL
  p <- fetchPage thisURL
  let pageData = getPage p
  let pageActions = (map pageAction pageData) <> carry
  let nextURL = nextPageID p
  case nextURL of
    Nothing -> withPages' normalizeURL fetchPage (Done pageActions) pageAction
    Just u' ->
      let uNorm = normalizeURL u' in
      withPages' normalizeURL fetchPage (Iter pageActions uNorm) pageAction

withPages :: (Monad m, MonadIO m, Paginated a b) =>
  String -> (String -> String) -> (String -> m a) -> (b -> m c) -> m [c]
withPages baseURL normalizeURL fetchPage =
  withPages' normalizeURL fetchPage (Iter [] baseURL)

getAllPages' ::
  (Monad m, MonadIO m, Paginated a b) =>
  String -> (String -> String) -> (String -> m a) -> m [b]
getAllPages' baseURL nextURL fetchPage =
  withPages baseURL nextURL fetchPage return
