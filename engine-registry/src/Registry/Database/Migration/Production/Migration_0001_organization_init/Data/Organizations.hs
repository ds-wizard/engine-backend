module Registry.Database.Migration.Production.Migration_0001_organization_init.Data.Organizations where

import qualified Data.Bson as BSON

organization now =
  [ "organizationId" BSON.=: "organization"
  , "name" BSON.=: "Organization name"
  , "description" BSON.=: "Some description of Organization"
  , "email" BSON.=: "organization@example.com"
  , "role" BSON.=: "AdminRole"
  , "token" BSON.=: "GlobalToken"
  , "active" BSON.=: True
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  , "lastAccessAt" BSON.=: now
  ]
