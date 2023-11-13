module Wizard.Database.Migration.Development.User.Data.IsaacNewton where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.AlbertEinstein
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup
import WizardLib.Public.Model.User.UserGroupMembership

userIsaac :: User
userIsaac =
  User
    { uuid = u' "e1c58e52-0824-4526-8ebe-ec38eec67030"
    , firstName = "Isaac"
    , lastName = "Newton"
    , email = "isaac.newton@example.com"
    , affiliation = Nothing
    , sources = [_USER_SOURCE_INTERNAL]
    , uRole = _USER_ROLE_RESEARCHER
    , permissions = ["PM_READ_PERM", "QTN_PERM", "DOC_TML_READ_PERM", "SUBM_PERM"]
    , active = True
    , passwordHash = "pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4="
    , submissionProps = []
    , imageUrl = Nothing
    , machine = False
    , tenantUuid = defaultTenant.uuid
    , lastVisitedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 22) 0
    }

userIsaacEdited :: User
userIsaacEdited =
  userAlbert
    { firstName = "EDITED: Isaac"
    , lastName = "EDITED: Newton"
    , email = "albert.einstein@example.com"
    , affiliation = Just "EDITED: My University"
    , uRole = _USER_ROLE_ADMIN
    , active = True
    }

userIsaacEditedChange :: UserChangeDTO
userIsaacEditedChange =
  UserChangeDTO
    { firstName = userIsaacEdited.firstName
    , lastName = userIsaacEdited.lastName
    , email = userIsaacEdited.email
    , affiliation = userIsaacEdited.affiliation
    , uRole = userIsaacEdited.uRole
    , active = userIsaacEdited.active
    }

userIsaacProfileChange :: UserProfileChangeDTO
userIsaacProfileChange =
  UserProfileChangeDTO
    { firstName = userAlbertEdited.firstName
    , lastName = userAlbertEdited.lastName
    , email = userAlbertEdited.email
    , affiliation = userAlbertEdited.affiliation
    }

userIsaacBioGroupMembership :: UserGroupMembership
userIsaacBioGroupMembership =
  UserGroupMembership
    { userGroupUuid = bioGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userIsaacPlantGroupMembership :: UserGroupMembership
userIsaacPlantGroupMembership =
  UserGroupMembership
    { userGroupUuid = plantGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

userIsaacAnimalGroupMembership :: UserGroupMembership
userIsaacAnimalGroupMembership =
  UserGroupMembership
    { userGroupUuid = animalGroup.uuid
    , userUuid = userIsaac.uuid
    , mType = MemberUserGroupMembershipType
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
