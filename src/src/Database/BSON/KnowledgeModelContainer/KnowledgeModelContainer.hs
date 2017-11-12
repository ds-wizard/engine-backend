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
    [ "uuid" BSON.=: serializeUUID (km ^. kmcKmContainerUuid)
    , "name" BSON.=: (km ^. kmcName)
    , "artefactId" BSON.=: (km ^. kmcArtefactId)
    , "parentPackageId" BSON.=: (km ^. kmcParentPackageId)
    ]

instance FromBSON KnowledgeModelContainer where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artefactId <- BSON.lookup "artefactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    return
      KnowledgeModelContainer
      { _kmcKmContainerUuid = uuid
      , _kmcName = name
      , _kmcArtefactId = artefactId
      , _kmcParentPackageId = parentPackageId
      }
