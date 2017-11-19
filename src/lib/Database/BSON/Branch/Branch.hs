module Database.BSON.Branch.Branch where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Branch.Branch

instance ToBSON Branch where
  toBSON branch =
    [ "uuid" BSON.=: serializeUUID (branch ^. bUuid)
    , "name" BSON.=: (branch ^. bName)
    , "artifactId" BSON.=: (branch ^. bArtifactId)
    , "parentPackageId" BSON.=: (branch ^. bParentPackageId)
    ]

instance FromBSON Branch where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    return
      Branch
      { _bUuid = uuid
      , _bName = name
      , _bArtifactId = artifactId
      , _bParentPackageId = parentPackageId
      }
