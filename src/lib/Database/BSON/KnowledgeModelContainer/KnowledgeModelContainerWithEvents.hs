module Database.BSON.KnowledgeModelContainer.KnowledgeModelContainerWithEvents where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Database.BSON.Event.Common
import Model.KnowledgeModelContainer.KnowledgeModelContainer

instance FromBSON KnowledgeModelContainerWithEvents where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    eventsSerialized <- BSON.lookup "events" doc
    let events = fmap (fromJust . chooseEventDeserializator) eventsSerialized
    return
      KnowledgeModelContainerWithEvents
      { _kmcweKmContainerUuid = uuid
      , _kmcweName = name
      , _kmcweArtifactId = artifactId
      , _kmcweParentPackageId = parentPackageId
      , _kmcweEvents = events
      }
