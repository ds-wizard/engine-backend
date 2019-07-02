module Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON (Object o) = do
    _migratorStateCreateDTOTargetPackageId <- o .: "targetPackageId"
    return MigratorStateCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateCreateDTO where
  toJSON MigratorStateCreateDTO {..} = object ["targetPackageId" .= _migratorStateCreateDTOTargetPackageId]
