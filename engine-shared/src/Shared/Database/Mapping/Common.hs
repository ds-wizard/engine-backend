module Shared.Database.Mapping.Common where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Typeable
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Text.Read (readMaybe)

toFieldGenericEnum :: Show enum => enum -> Action
toFieldGenericEnum = Escape . BS.pack . show

fromFieldGenericEnum :: (Read enum, Typeable enum) => Field -> Maybe BS.ByteString -> Conversion enum
fromFieldGenericEnum f dat =
  case fmap (readMaybe . BS.unpack) dat of
    Just (Just role) -> return role
    _ -> returnError ConversionFailed f "Unsupported type"

toStringField :: String -> Action
toStringField = Escape . BS.pack

data PostgresEmbedded a =
  PostgresEmbedded
    { _postgresEmbeddedData :: a
    }
  deriving (Generic)

instance (FromJSON a, Generic a) => FromJSON (PostgresEmbedded a)

instance (ToJSON a, Generic a) => ToJSON (PostgresEmbedded a)
