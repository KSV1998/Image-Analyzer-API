module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
    ( FromRow)
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.FromRow ( FromRow(..), field )
import Prelude

newtype IName = IName
  { en :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype IData = IData
  { tag :: IName
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Results = Results
  { tags :: [IData]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype IResponse = IResponse
  { result :: Results
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Image = Image
    {
        id :: UUID,
        label :: String,
        detection :: Bool,
        objects :: Maybe [String],
        uri :: String
    } deriving(Generic, ToJSON, Show)

data RequestBody = RequestBody {
  url :: String,
  label :: Maybe String,
  is_det :: Bool,
  source :: String
  }  deriving(Generic, ToJSON, FromJSON)

type AuthCode = ByteString
type UserId = ByteString
type DbName = ByteString
type Password = ByteString

instance FromRow Text where
    fromRow = field

instance FromRow UUID where
    fromRow = field
