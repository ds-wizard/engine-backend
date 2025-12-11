module Wizard.Database.Migration.Development.Project.Data.ProjectVersions where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Tenant (defaultTenantUuid)
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Model.User.User
import Wizard.Service.Project.Version.ProjectVersionMapper

qVersions :: U.UUID -> [ProjectVersion]
qVersions projectUuid = [projectVersion1 projectUuid]

qVersionsList :: U.UUID -> [ProjectVersionList]
qVersionsList projectUuid = [projectVersion1List projectUuid]

projectVersion1 :: U.UUID -> ProjectVersion
projectVersion1 projectUuid =
  ProjectVersion
    { uuid = createVersionUuid projectUuid "dd016270ce7e"
    , name = "Version 1"
    , description = Just "Version 1 description"
    , eventUuid = (slble_rQ1 projectUuid).uuid
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

projectVersion1List :: U.UUID -> ProjectVersionList
projectVersion1List projectUuid = toVersionList (projectVersion1 projectUuid) (Just userAlbertDto)

projectVersion1Edited :: U.UUID -> ProjectVersion
projectVersion1Edited projectUuid =
  (projectVersion1 projectUuid)
    { name = "EDITED: " ++ (projectVersion1 projectUuid).name
    , description = fmap ("EDITED: " ++) (projectVersion1 projectUuid).description
    , eventUuid = (sre_rQ11 projectUuid).uuid
    }

projectVersion1EditedList :: U.UUID -> ProjectVersionList
projectVersion1EditedList projectUuid = toVersionList (projectVersion1Edited projectUuid) (Just userAlbertDto)

projectVersion1EditedChangeDto :: U.UUID -> ProjectVersionChangeDTO
projectVersion1EditedChangeDto projectUuid = toVersionChangeDTO (projectVersion1Edited projectUuid)

projectVersion1RevertDto :: U.UUID -> ProjectVersionRevertDTO
projectVersion1RevertDto projectUuid = toVersionRevertDTO (sre_rQ2 projectUuid).uuid

projectVersion2 :: U.UUID -> ProjectVersion
projectVersion2 projectUuid =
  ProjectVersion
    { uuid = createVersionUuid projectUuid "515f1d45b24f"
    , name = "Version 2"
    , description = Just "Version 2 description"
    , eventUuid = (sre_rQ11 projectUuid).uuid
    , projectUuid = projectUuid
    , tenantUuid = defaultTenantUuid
    , createdBy = Just userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }
projectVersion2ChangeDto :: U.UUID -> ProjectVersionChangeDTO
projectVersion2ChangeDto projectUuid = toVersionChangeDTO (projectVersion2 projectUuid)

createVersionUuid :: U.UUID -> String -> U.UUID
createVersionUuid projectUuid eventSuffix =
  let parts = splitOn "-" . U.toString $ projectUuid
   in u' . L.intercalate "-" $ [head parts, parts !! 1, parts !! 2, parts !! 3, eventSuffix]
