module Api.Resource.Migrator.MigratorStateDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Migrator.Common ()
import Model.Migrator.MigratorState

data MigratorStateDTO = MigratorStateDTO
  { _msdtoBranchUuid :: U.UUID
  , _msdtoMigrationState :: MigrationState
  , _msdtoBranchParentId :: String
  , _msdtoTargetPackageId :: String
  , _msdtoCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
  } deriving (Show, Eq)

makeLenses ''MigratorStateDTO

instance FromJSON MigratorStateDTO where
  parseJSON (Object o) = do
    _msdtoBranchUuid <- o .: "branchUuid"
    _msdtoMigrationState <- o .: "migrationState"
    _msdtoBranchParentId <- o .: "branchParentId"
    _msdtoTargetPackageId <- o .: "targetPackageId"
    _msdtoCurrentKnowledgeModel <- o .: "currentKnowledgeModel"
    return MigratorStateDTO {..}
  parseJSON _ = mzero

instance ToJSON MigratorStateDTO where
  toJSON MigratorStateDTO {..} =
    object
      [ "branchUuid" .= _msdtoBranchUuid
      , "migrationState" .= _msdtoMigrationState
      , "branchParentId" .= _msdtoBranchParentId
      , "targetPackageId" .= _msdtoTargetPackageId
      , "currentKnowledgeModel" .= _msdtoCurrentKnowledgeModel
      ]
