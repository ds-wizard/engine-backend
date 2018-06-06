module Api.Resource.Migrator.MigratorStateCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Migrator.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON (Object o) = do
    _migratorStateCreateDTOTargetPackageId <- o .: "targetPackageId"
    return MigratorStateCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateCreateDTO where
  toJSON MigratorStateCreateDTO {..} = object ["targetPackageId" .= _migratorStateCreateDTOTargetPackageId]
