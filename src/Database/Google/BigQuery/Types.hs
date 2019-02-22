{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Google.BigQuery.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Map.Strict
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

class Paginated a b | a -> b where
  nextPageID :: a -> Maybe String
  getPage :: a -> [b]

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

data PaginatedResults a = PaginatedResults
  { _paginatedKind :: T.Text
  , _paginatedETag :: T.Text
  , _paginatedNextPageToken :: Maybe String
  , _paginatedValues :: [a]
  } deriving (Eq, Show, Ord)

class NamedKey a where
  keyName :: T.Text

instance NamedKey Dataset where keyName = "datasets"

instance (FromJSON a, NamedKey a) => FromJSON (PaginatedResults a) where
    parseJSON (Object v) = PaginatedResults
      <$> v .:  "kind"
      <*> v .:  "etag"
      <*> v .:? "nextPageToken"
      <*> v .:  keyName @a
    parseJSON invalid = typeMismatch "paginated_results" invalid

instance Paginated (PaginatedResults a) a where
  nextPageID = _paginatedNextPageToken
  getPage = _paginatedValues

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

getAllPages :: (Monad m, Paginated a b) => String -> (String -> String) -> (String -> m a) -> m [b]
getAllPages = getAllPages' []
  where
    getAllPages' ::
      (Monad m, Paginated a b) =>
      [b] -> String -> (String -> String) -> (String -> m a) -> m [b]
    getAllPages' carry s nextS nextF = do
      page <- nextF s
      let carry' = carry <> getPage page
      case nextPageID page of
        Nothing -> return carry'
        Just nextID -> do
          getAllPages' carry' (nextS nextID) nextS nextF
