module Api.Resources.Migrator.MigratorStateCreateDTO where

import Control.Lens ((^.), makeLenses)
import Control.Monad
import Data.Aeson
import Data.UUID

import Common.Uuid

data MigratorStateCreateDTO = MigratorStateCreateDTO
  { _mscdtoTargetPackageId :: String
  } deriving (Show, Eq)

makeLenses ''MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON (Object o) = do
    _mscdtoTargetPackageId <- o .: "targetPackageId"
    return MigratorStateCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateCreateDTO where
  toJSON MigratorStateCreateDTO {..} = object ["targetPackageId" .= _mscdtoTargetPackageId]
