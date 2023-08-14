module WizardLib.KnowledgeModel.Api.Resource.Package.PackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO
import WizardLib.KnowledgeModel.Api.Resource.Package.PackagePhaseJM ()
import WizardLib.KnowledgeModel.Model.Package.Package

instance ToJSON PackageDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PackageDTO where
  parseJSON (Object o) = do
    pId <- o .: "id"
    name <- o .: "name"
    organizationId <- o .: "organizationId"
    kmId <- o .: "kmId"
    version <- o .: "version"
    phase <- o .:? "phase" .!= ReleasedPackagePhase
    metamodelVersion <- o .: "metamodelVersion"
    description <- o .: "description"
    readme <- o .:? "readme" .!= ""
    license <- o .:? "license" .!= ""
    parentPackageId <- o .:? "parentPackageId"
    previousPackageId <- o .:? "previousPackageId" .!= parentPackageId
    forkOfPackageId <- o .:? "forkOfPackageId" .!= parentPackageId
    mergeCheckpointPackageId <- o .:? "mergeCheckpointPackageId" .!= parentPackageId
    eventSerialized <- o .: "events"
    events <- parseJSON eventSerialized
    let nonEditable = False
    createdAt <- o .:? "createdAt" .!= UTCTime (fromJust $ fromGregorianValid 1970 1 1) 0
    return PackageDTO {..}
  parseJSON _ = mzero
