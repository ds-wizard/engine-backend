module Database.Migration.Production.Migration_0001_organization_init.Data.Organizations where

import qualified Data.Bson as BSON

organization now =
  [ "organizationId" BSON.=: "dsw"
  , "name" BSON.=: "DSW"
  , "description" BSON.=: "Some description of DSW"
  , "email" BSON.=: "dsw@example.com"
  , "role" BSON.=: "AdminRole"
  , "token" BSON.=: "DswToken"
  , "active" BSON.=: True
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  , "lastAccessAt" BSON.=: now
  ]
