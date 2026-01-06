module Wizard.Service.Project.Version.ProjectVersionMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList
import qualified Wizard.Service.User.UserMapper as UM

toVersionList :: ProjectVersion -> Maybe UserDTO -> ProjectVersionList
toVersionList version createdBy =
  ProjectVersionList
    { uuid = version.uuid
    , name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    , createdBy = fmap UM.toSuggestion' createdBy
    , createdAt = version.createdAt
    , updatedAt = version.updatedAt
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionChangeDTO :: ProjectVersion -> ProjectVersionChangeDTO
toVersionChangeDTO version =
  ProjectVersionChangeDTO
    { name = version.name
    , description = version.description
    , eventUuid = version.eventUuid
    }

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
toVersionRevertDTO :: U.UUID -> ProjectVersionRevertDTO
toVersionRevertDTO eventUuid = ProjectVersionRevertDTO {eventUuid = eventUuid}

-- ---------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------
fromVersionChangeDTO :: ProjectVersionChangeDTO -> U.UUID -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> ProjectVersion
fromVersionChangeDTO reqDto uuid projectUuid tenantUuid createdBy now =
  ProjectVersion
    { uuid = uuid
    , name = reqDto.name
    , description = reqDto.description
    , eventUuid = reqDto.eventUuid
    , projectUuid = projectUuid
    , tenantUuid = tenantUuid
    , createdBy = Just createdBy
    , createdAt = now
    , updatedAt = now
    }

fromVersionChangeDTO' :: ProjectVersion -> ProjectVersionChangeDTO -> UTCTime -> ProjectVersion
fromVersionChangeDTO' version reqDto now =
  ProjectVersion
    { uuid = version.uuid
    , name = reqDto.name
    , description = reqDto.description
    , eventUuid = reqDto.eventUuid
    , projectUuid = version.projectUuid
    , tenantUuid = version.tenantUuid
    , createdBy = version.createdBy
    , createdAt = version.createdAt
    , updatedAt = now
    }
