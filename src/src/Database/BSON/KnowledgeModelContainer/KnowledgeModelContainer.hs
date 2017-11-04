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
    , "shortName" BSON.=: (km ^. kmcShortName)
    , "parentPackageName" BSON.=: (km ^. kmcParentPackageName)
    , "parentPackageVersion" BSON.=: (km ^. kmcParentPackageVersion)
    ]

instance FromBSON KnowledgeModelContainer where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    shortName <- BSON.lookup "shortName" doc
    parentPackageName <- BSON.lookup "parentPackageName" doc
    parentPackageVersion <- BSON.lookup "parentPackageVersion" doc
    return
      KnowledgeModelContainer
      { _kmcKmContainerUuid = uuid
      , _kmcName = name
      , _kmcShortName = shortName
      , _kmcParentPackageName = parentPackageName
      , _kmcParentPackageVersion = parentPackageVersion
      }
