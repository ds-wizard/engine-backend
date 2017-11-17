module Database.BSON.KnowledgeModelContainer.KnowledgeModelContainer where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.KnowledgeModelContainer.KnowledgeModelContainer

instance ToBSON KnowledgeModelContainer where
  toBSON km =
    [ "uuid" BSON.=: serializeUUID (km ^. kmcKmcUuid)
    , "name" BSON.=: (km ^. kmcName)
    , "artifactId" BSON.=: (km ^. kmcArtifactId)
    , "parentPackageId" BSON.=: (km ^. kmcParentPackageId)
    ]

instance FromBSON KnowledgeModelContainer where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    return
      KnowledgeModelContainer
      { _kmcKmcUuid = uuid
      , _kmcName = name
      , _kmcArtifactId = artifactId
      , _kmcParentPackageId = parentPackageId
      }
