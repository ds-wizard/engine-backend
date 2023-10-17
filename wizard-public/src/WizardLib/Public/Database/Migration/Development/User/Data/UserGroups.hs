module WizardLib.Public.Database.Migration.Development.User.Data.UserGroups where

import Shared.Common.Constant.Tenant
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import WizardLib.Public.Model.User.UserGroup

bioGroup :: UserGroup
bioGroup =
  UserGroup
    { uuid = u' "6ab8ecc3-c0f5-4864-a055-b0096ca55569"
    , name = "Bio Group"
    , description = Just "Some description about bio group"
    , private = True
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

plantGroup :: UserGroup
plantGroup =
  UserGroup
    { uuid = u' "e249180a-a472-4f7e-8df9-7de1bb330360"
    , name = "Plant Group"
    , description = Just "Some description about plant group"
    , private = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

animalGroup :: UserGroup
animalGroup =
  UserGroup
    { uuid = u' "37c14f56-a4b6-4f74-a43a-3a757eb22a38"
    , name = "Animal Group"
    , description = Nothing
    , private = False
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
