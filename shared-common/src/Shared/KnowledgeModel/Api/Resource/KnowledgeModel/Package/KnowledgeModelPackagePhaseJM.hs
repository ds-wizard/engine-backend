module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM where

import Data.Aeson
import qualified Data.Text as T
import Servant.API
import Text.Read (readMaybe)

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToJSON KnowledgeModelPackagePhase

instance FromJSON KnowledgeModelPackagePhase

instance FromHttpApiData KnowledgeModelPackagePhase where
  parseQueryParam a =
    case readMaybe (T.unpack a) of
      Just phase -> Right phase
      Nothing -> Left "Unable to parse KnowledgeModelPackagePhase"
