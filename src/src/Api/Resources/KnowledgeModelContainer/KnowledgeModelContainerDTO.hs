module Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data KnowledgeModelContainerDTO = KnowledgeModelContainerDTO
  { _kmcdtoKmContainerUuid :: UUID
  , _kmcdtoName :: String
  , _kmcdtoArtifactId :: String
  , _kmcdtoParentPackageId :: Maybe String
  }

makeLenses ''KnowledgeModelContainerDTO

instance FromJSON KnowledgeModelContainerDTO where
  parseJSON (Object o) = do
    _kmcdtoKmContainerUuid <- o .: "uuid"
    _kmcdtoName <- o .: "name"
    _kmcdtoArtifactId <- o .: "artifactId"
    _kmcdtoParentPackageId <- o .: "parentPackageId"
    return KnowledgeModelContainerDTO {..}
  parseJSON _ = mzero

instance ToJSON KnowledgeModelContainerDTO where
  toJSON KnowledgeModelContainerDTO {..} =
    object
      [ "uuid" .= _kmcdtoKmContainerUuid
      , "name" .= _kmcdtoName
      , "artifactId" .= _kmcdtoArtifactId
      , "parentPackageId" .= _kmcdtoParentPackageId
      ]
