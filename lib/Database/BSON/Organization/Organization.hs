module Database.BSON.Organization.Organization where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import LensesConfig
import Model.Organization.Organization

instance ToBSON Organization where
  toBSON organization =
    [ "uuid" BSON.=: (organization ^. uuid)
    , "name" BSON.=: (organization ^. name)
    , "organizationId" BSON.=: (organization ^. organizationId)
    , "createdAt" BSON.=: (organization ^. createdAt)
    , "updatedAt" BSON.=: (organization ^. updatedAt)
    ]

instance FromBSON Organization where
  fromBSON doc = do
    orgUuid <- BSON.lookup "uuid" doc
    orgName <- BSON.lookup "name" doc
    orgOrganizationId <- BSON.lookup "organizationId" doc
    orgCreatedAt <- BSON.lookup "createdAt" doc
    orgUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      Organization
      { _organizationUuid = orgUuid
      , _organizationName = orgName
      , _organizationOrganizationId = orgOrganizationId
      , _organizationCreatedAt = orgCreatedAt
      , _organizationUpdatedAt = orgUpdatedAt
      }
