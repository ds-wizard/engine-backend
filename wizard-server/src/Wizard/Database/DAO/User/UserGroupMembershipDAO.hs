module Wizard.Database.DAO.User.UserGroupMembershipDAO where

import qualified Data.UUID as U

import Shared.Common.Database.DAO.Common
import Wizard.Database.Mapping.User.UserGroupSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Shared.Common.Database.Mapping.Common ()

entityName = "user_group_membership"

findUserGroupUuidsForUserUuidAndTenantUuid :: U.UUID -> U.UUID -> AppContextM [U.UUID]
findUserGroupUuidsForUserUuidAndTenantUuid userUuid tenantUuid =
  createFindEntitiesWithFieldsByFn "user_group_uuid" entityName [("user_uuid", U.toString userUuid), ("tenant_uuid", U.toString tenantUuid)]
