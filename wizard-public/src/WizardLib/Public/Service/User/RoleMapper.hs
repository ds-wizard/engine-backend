module WizardLib.Public.Service.User.RoleMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.User.RoleChangeDTO
import WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateRoleCommand
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.RoleList
import WizardLib.Public.Model.User.RoleSimple

toDTO :: Role -> Int -> RoleList
toDTO role usersCount =
  RoleList
    { uuid = role.uuid
    , name = role.name
    , permissions = role.permissions
    , usersCount = usersCount
    , isAdmin = role.isAdmin
    }

toRoleSimple :: Role -> RoleSimple
toRoleSimple role =
  RoleSimple
    { uuid = role.uuid
    , name = role.name
    , permissions = role.permissions
    }

fromCreateDTO :: RoleChangeDTO -> U.UUID -> U.UUID -> UTCTime -> Role
fromCreateDTO dto uuid tenantUuid now =
  Role
    { uuid = uuid
    , name = dto.name
    , permissions = dto.permissions
    , isAdmin = False
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: Role -> RoleChangeDTO -> UTCTime -> Role
fromChangeDTO role dto now =
  Role
    { uuid = role.uuid
    , name = dto.name
    , permissions = dto.permissions
    , isAdmin = role.isAdmin
    , tenantUuid = role.tenantUuid
    , createdAt = role.createdAt
    , updatedAt = now
    }

fromCommandCreate :: CreateOrUpdateRoleCommand -> U.UUID -> UTCTime -> Role
fromCommandCreate command tenantUuid now =
  Role
    { uuid = command.uuid
    , name = command.name
    , permissions = command.permissions
    , isAdmin = command.isAdmin
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromCommandUpdate :: Role -> CreateOrUpdateRoleCommand -> UTCTime -> Role
fromCommandUpdate role command now =
  Role
    { uuid = role.uuid
    , name = command.name
    , permissions = command.permissions
    , isAdmin = command.isAdmin
    , tenantUuid = role.tenantUuid
    , createdAt = role.createdAt
    , updatedAt = now
    }
