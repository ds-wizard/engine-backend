module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM where

import Control.Monad
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToJSON KnowledgeModelBundlePackage where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelBundlePackage where
  parseJSON (Object o) = do
    pId <- o .: "id"
    name <- o .: "name"
    organizationId <- o .: "organizationId"
    kmId <- o .: "kmId"
    version <- o .: "version"
    let phase = ReleasedKnowledgeModelPackagePhase
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
    return KnowledgeModelBundlePackage {..}
  parseJSON _ = mzero
