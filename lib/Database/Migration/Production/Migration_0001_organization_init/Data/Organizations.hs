module Database.Migration.Production.Migration_0001_organization_init.Data.Organizations where

import qualified Data.Bson as BSON

organization now =
  ["name" BSON.=: "ELIXIR Global", "organizationId" BSON.=: "elixir", "createdAt" BSON.=: now, "updatedAt" BSON.=: now]
