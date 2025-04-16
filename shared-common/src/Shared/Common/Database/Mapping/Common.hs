module Shared.Common.Database.Mapping.Common where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Typeable
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import GHC.Generics
import Text.Read (readMaybe)

import Shared.Common.Util.String

toFieldGenericEnum :: Show enum => enum -> Action
toFieldGenericEnum = Escape . BS.pack . show

fromFieldGenericEnum :: (Read enum, Typeable enum) => Field -> Maybe BS.ByteString -> Conversion enum
fromFieldGenericEnum f dat =
  case fmap (readMaybe . BS.unpack) dat of
    Just (Just role) -> return role
    _ -> returnError ConversionFailed f "Unsupported type"

toStringField :: String -> Action
toStringField = Escape . BS.pack

data PostgresEmbedded a = PostgresEmbedded
  { _postgresEmbeddedData :: a
  }
  deriving (Generic)

instance (FromJSON a, Generic a) => FromJSON (PostgresEmbedded a)

instance (ToJSON a, Generic a) => ToJSON (PostgresEmbedded a)

instance FromField (M.Map String String) where
  fromField f entity =
    case fmap (eitherDecode . BSL.fromStrict) entity of
      (Just (Right decodedEntity)) -> return decodedEntity
      _ -> returnError ConversionFailed f "Unsupported type"

instance ToField (M.Map String String) where
  toField = Escape . BSL.toStrict . encode

instance FromRow U.UUID where
  fromRow = field

instance FromRow Value where
  fromRow = fieldWith fromJSONField

instance ToField [U.UUID] where
  toField entities =
    let encoded = f' "{%s}" [L.intercalate "," . fmap U.toString $ entities]
     in Escape . BS.pack $ encoded

instance ToField [String] where
  toField entities =
    let encoded = f' "{%s}" [L.intercalate "," entities]
     in Escape . BS.pack $ encoded

instance FromField [String] where
  fromField f mdata = do
    pgArray <- fromField f mdata :: Conversion (PGArray T.Text)
    return (map T.unpack (fromPGArray pgArray))
