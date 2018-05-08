module Database.BSON.Organization.Organization where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import LensesConfig
import Model.Organization.Organization

instance ToBSON Organization where
  toBSON organization =
    [ "uuid" BSON.=: serializeUUID (organization ^. uuid)
    , "name" BSON.=: (organization ^. name)
    , "organizationId" BSON.=: (organization ^. organizationId)
    ]

instance FromBSON Organization where
  fromBSON doc = do
    orgUuid <- deserializeUUID $ BSON.lookup "uuid" doc
    orgName <- BSON.lookup "name" doc
    orgOrganizationId <- BSON.lookup "organizationId" doc
    return
      Organization
      {_organizationUuid = orgUuid, _organizationName = orgName, _organizationOrganizationId = orgOrganizationId}
