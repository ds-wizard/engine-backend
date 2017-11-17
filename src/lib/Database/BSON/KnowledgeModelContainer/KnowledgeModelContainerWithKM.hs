module Database.BSON.KnowledgeModelContainer.KnowledgeModelContainerWithKM where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer

instance FromBSON KnowledgeModelContainerWithKM where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    kmSerialized <- BSON.lookup "knowledgeModel" doc
--    km <- fromBSON kmSerialized
    let km = deserializeKM kmSerialized
    return
      KnowledgeModelContainerWithKM
      { _kmcwkmKmContainerUuid = uuid
      , _kmcwkmName = name
      , _kmcwkmArtifactId = artifactId
      , _kmcwkmParentPackageId = parentPackageId
      , _kmcwkmKM = km
      }
    where
      deserializeKM (Just kmSerialized) = fromBSON kmSerialized
      deserializeKM Nothing = Nothing
