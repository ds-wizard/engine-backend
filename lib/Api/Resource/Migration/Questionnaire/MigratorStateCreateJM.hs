module Api.Resource.Migration.Questionnaire.MigratorStateCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO

instance FromJSON MigratorStateCreateDTO where
  parseJSON (Object o) = do
    _migratorStateCreateDTOTargetPackageId <- o .: "targetPackageId"
    _migratorStateCreateDTOTargetTagUuids <- o .: "targetTagUuids"
    return MigratorStateCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateCreateDTO where
  toJSON MigratorStateCreateDTO {..} =
    object
      [ "targetPackageId" .= _migratorStateCreateDTOTargetPackageId
      , "targetTagUuids" .= _migratorStateCreateDTOTargetTagUuids
      ]
