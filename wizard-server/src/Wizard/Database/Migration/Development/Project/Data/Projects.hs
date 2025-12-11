module Wizard.Database.Migration.Development.Project.Data.Projects where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.Date
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Project.Data.ProjectComments
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectLabels
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectState
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.Project.ProjectMapper
import WizardLib.Public.Database.Migration.Development.User.Data.UserGroups
import WizardLib.Public.Model.User.UserGroup

_PROJECT_TAG_1 = "projectTag1"

_PROJECT_TAG_2 = "projectTag2"

project1Uuid :: U.UUID
project1Uuid = u' "af984a75-56e3-49f8-b16f-d6b99599910a"

project1 :: Project
project1 =
  Project
    { uuid = project1Uuid
    , name = "My Private Project"
    , description = Just "Some description"
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_PROJECT_TAG_1]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [project1AlbertEditProjectPerm]
    , isTemplate = True
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

project1Events :: [ProjectEvent]
project1Events = fEvents project1Uuid

project1Edited :: Project
project1Edited =
  project1
    { name = "EDITED: " ++ project1.name
    , visibility = VisibleEditProjectVisibility
    , sharing = RestrictedProjectSharing
    , projectTags = [_PROJECT_TAG_1, _PROJECT_TAG_2]
    , permissions = []
    }

project1ShareEdited :: Project
project1ShareEdited =
  project1
    { visibility = VisibleEditProjectVisibility
    , sharing = RestrictedProjectSharing
    , permissions = []
    }

project1SettingsEdited :: Project
project1SettingsEdited =
  project1
    { name = "EDITED: " ++ project1.name
    , projectTags = [_PROJECT_TAG_1, _PROJECT_TAG_2]
    }

project1EventsEdited :: [ProjectEvent]
project1EventsEdited = fEventsEdited project1Uuid

project1Versions :: [ProjectVersion]
project1Versions = qVersions project1Uuid

project1Ctn :: ProjectContent
project1Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project1CtnRevertedDto :: ProjectContentDTO
project1CtnRevertedDto =
  ProjectContentDTO
    { phaseUuid = Nothing
    , replies = M.fromList [rQ1, rQ2]
    , commentThreadsMap = projectCommentThreadsList
    , labels = M.empty
    , events = [toEventList (sre_rQ1' project1Uuid) (Just userAlbert), toEventList (sre_rQ2' project1Uuid) (Just userAlbert)]
    , versions = []
    }

project1Dto :: ProjectDTO
project1Dto = toSimpleDTO project1 germanyKmPackage DefaultProjectState [project1AlbertEditProjectPermDto]

project1Create :: ProjectCreateDTO
project1Create =
  ProjectCreateDTO
    { name = project1.name
    , knowledgeModelPackageId = project1.knowledgeModelPackageId
    , visibility = project1.visibility
    , sharing = project1.sharing
    , questionTagUuids = []
    , documentTemplateId = project1.documentTemplateId
    , formatUuid = project1.formatUuid
    }

project1EditedShareChange :: ProjectShareChangeDTO
project1EditedShareChange =
  ProjectShareChangeDTO
    { visibility = project1ShareEdited.visibility
    , sharing = project1ShareEdited.sharing
    , permissions = fmap toProjectPermChangeDTO project1ShareEdited.permissions
    }

project1SettingsChange :: ProjectSettingsChangeDTO
project1SettingsChange =
  ProjectSettingsChangeDTO
    { name = project1SettingsEdited.name
    , description = project1SettingsEdited.description
    , projectTags = project1SettingsEdited.projectTags
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = project1SettingsEdited.isTemplate
    }

project1AlbertEditProjectPerm :: ProjectPerm
project1AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project1.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project1AlbertEditProjectPermDto :: ProjectPermDTO
project1AlbertEditProjectPermDto = toUserProjectPermDTO project1AlbertEditProjectPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project2Uuid :: U.UUID
project2Uuid = u' "d57520b4-5a70-4d40-8623-af2bfbbdfdfe"

project2 :: Project
project2 =
  Project
    { uuid = project2Uuid
    , name = "My VisibleView Project"
    , description = Just "Some description"
    , visibility = VisibleViewProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = [_PROJECT_TAG_1, _PROJECT_TAG_2]
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = [project2AlbertEditProjectPerm]
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 22
    }

project2Edited :: Project
project2Edited =
  Project
    { uuid = project2.uuid
    , name = "EDITED: " ++ project2.name
    , description = Just "EDITED: Some description"
    , visibility = VisibleEditProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = project2.knowledgeModelPackageId
    , selectedQuestionTagUuids = project2.selectedQuestionTagUuids
    , projectTags = project2.projectTags
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = project2.createdAt
    , updatedAt = project2.updatedAt
    }

project2Events :: [ProjectEvent]
project2Events = fEvents project2Uuid

project2Versions :: [ProjectVersion]
project2Versions = qVersions project2Uuid

project2ShareEdited :: Project
project2ShareEdited =
  project2
    { visibility = VisibleEditProjectVisibility
    , sharing = RestrictedProjectSharing
    , permissions = []
    }

project2SettingsEdited :: Project
project2SettingsEdited =
  project2
    { name = "EDITED: " ++ project2.name
    , description = Just "EDITED: Some description"
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userAlbert.uuid
    , isTemplate = False
    }

project2Ctn :: ProjectContent
project2Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project2EventsEdited :: [ProjectEvent]
project2EventsEdited = fEventsEdited project2Uuid

project2Dto :: ProjectDTO
project2Dto = toSimpleDTO project2 germanyKmPackage DefaultProjectState [project2AlbertEditProjectPermDto]

project2AlbertEditProjectPerm :: ProjectPerm
project2AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project2.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project2AlbertEditProjectPermDto :: ProjectPermDTO
project2AlbertEditProjectPermDto = toUserProjectPermDTO project2AlbertEditProjectPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project3Uuid :: U.UUID
project3Uuid = u' "16530a07-e673-4ff3-ac1f-57250f2c1bfe"

project3 :: Project
project3 =
  Project
    { uuid = project3Uuid
    , name = "My VisibleEdit Project"
    , description = Just "Some description"
    , visibility = VisibleEditProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 28
    }

project3Events :: [ProjectEvent]
project3Events = fEvents project3Uuid

project3Versions :: [ProjectVersion]
project3Versions = qVersions project3Uuid

project3Ctn :: ProjectContent
project3Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project3EventsEdited :: [ProjectEvent]
project3EventsEdited = fEventsEdited project3Uuid

project3Dto :: ProjectDTO
project3Dto = toSimpleDTO project3 germanyKmPackage DefaultProjectState []

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project4Uuid :: U.UUID
project4Uuid = u' "57250a07-a663-4ff3-ac1f-16530f2c1bfe"

project4 :: Project
project4 =
  Project
    { uuid = project4Uuid
    , name = "Outdated Project"
    , description = Just "Some description"
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = netherlandsKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ wizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Nothing
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

project4Events :: [ProjectEvent]
project4Events = [sphse_2' project4Uuid]

project4Versions :: [ProjectVersion]
project4Versions = []

project4Ctn :: ProjectContent
project4Ctn =
  ProjectContent
    { phaseUuid = Just $ phase2.uuid
    , replies = M.empty
    , labels = M.empty
    }

project4VisibleView :: Project
project4VisibleView = project4 {visibility = VisibleViewProjectVisibility}

project4VisibleViewEvents :: [ProjectEvent]
project4VisibleViewEvents = [sphse_2' project4VisibleView.uuid]

project4VisibleEdit :: Project
project4VisibleEdit = project4 {visibility = VisibleEditProjectVisibility}

project4VisibleEditEvents :: [ProjectEvent]
project4VisibleEditEvents = [sphse_2' project4VisibleEdit.uuid]

project4Upgraded :: Project
project4Upgraded =
  project4
    { uuid = u' "5deabef8-f526-421c-90e2-dd7aed1a25c5"
    , knowledgeModelPackageId = netherlandsKmPackageV2.pId
    }

project4UpgradedEvents :: [ProjectEvent]
project4UpgradedEvents = [sphse_2' project4Upgraded.uuid]

project4VisibleViewUpgraded :: Project
project4VisibleViewUpgraded = project4Upgraded {visibility = VisibleViewProjectVisibility}

project4VisibleViewUpgradedEvents :: [ProjectEvent]
project4VisibleViewUpgradedEvents = [sphse_2' project4VisibleViewUpgraded.uuid]

project4VisibleEditUpgraded :: Project
project4VisibleEditUpgraded = project4Upgraded {visibility = VisibleEditProjectVisibility}

project4VisibleEditUpgradedEvents :: [ProjectEvent]
project4VisibleEditUpgradedEvents = [sphse_2' project4VisibleEditUpgraded.uuid]

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project5Uuid :: U.UUID
project5Uuid = u' "506be867-ba92-4e10-8175-187e99613366"

project5 :: Project
project5 =
  project1
    { uuid = project5Uuid
    , name = "My Private Project SharedView"
    , visibility = PrivateProjectVisibility
    , sharing = AnyoneWithLinkViewProjectSharing
    , permissions = [project5AlbertEditProjectPerm]
    }

project5Events :: [ProjectEvent]
project5Events = fEvents project5Uuid

project5EventsEdited :: [ProjectEvent]
project5EventsEdited = fEventsEdited project5Uuid

project5Versions :: [ProjectVersion]
project5Versions = qVersions project5Uuid

project5AlbertEditProjectPerm :: ProjectPerm
project5AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project5.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project6Uuid :: U.UUID
project6Uuid = u' "09304abd-2035-4046-8dc8-b3e5ba8c016c"

project6 :: Project
project6 =
  project1
    { uuid = project6Uuid
    , name = "My Private Project SharedEdit"
    , visibility = PrivateProjectVisibility
    , sharing = AnyoneWithLinkEditProjectSharing
    , permissions = [project6AlbertEditProjectPerm]
    }

project6Events :: [ProjectEvent]
project6Events = fEvents project6Uuid

project6Versions :: [ProjectVersion]
project6Versions = qVersions project6Uuid

project6Ctn :: ProjectContent
project6Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project6EventsEdited :: [ProjectEvent]
project6EventsEdited = fEventsEdited project6Uuid

project6Dto :: ProjectDTO
project6Dto = toSimpleDTO project6 germanyKmPackage DefaultProjectState [project6AlbertEditProjectPermDto]

project6AlbertEditProjectPerm :: ProjectPerm
project6AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project6.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project6AlbertEditProjectPermDto :: ProjectPermDTO
project6AlbertEditProjectPermDto = toUserProjectPermDTO project6AlbertEditProjectPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project7Uuid :: U.UUID
project7Uuid = u' "abd22b10-63fd-4cb8-bb23-7997ff32eccc"

project7 :: Project
project7 =
  project2
    { uuid = project7Uuid
    , name = "My VisibleView Project SharedView"
    , visibility = VisibleViewProjectVisibility
    , sharing = AnyoneWithLinkViewProjectSharing
    , permissions = [project7AlbertEditProjectPerm]
    }

project7Events :: [ProjectEvent]
project7Events = fEvents project7Uuid

project7EventsEdited :: [ProjectEvent]
project7EventsEdited = fEventsEdited project8Uuid

project7Versions :: [ProjectVersion]
project7Versions = qVersions project7Uuid

project7Ctn :: ProjectContent
project7Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project7AlbertEditProjectPerm :: ProjectPerm
project7AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project7.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project7AlbertEditProjectPermDto :: ProjectPermDTO
project7AlbertEditProjectPermDto = toUserProjectPermDTO project7AlbertEditProjectPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project8Uuid :: U.UUID
project8Uuid = u' "a990f62a-ca1f-4517-82d4-399951b8630b"

project8 :: Project
project8 =
  project2
    { uuid = project8Uuid
    , name = "My VisibleView Project SharedEdit"
    , visibility = VisibleViewProjectVisibility
    , sharing = AnyoneWithLinkEditProjectSharing
    , permissions = [project8AlbertEditProjectPerm]
    }

project8Events :: [ProjectEvent]
project8Events = fEvents project8Uuid

project8EventsEdited :: [ProjectEvent]
project8EventsEdited = fEventsEdited project8Uuid

project8Versions :: [ProjectVersion]
project8Versions = qVersions project8Uuid

project8AlbertEditProjectPerm :: ProjectPerm
project8AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project8.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project9Uuid :: U.UUID
project9Uuid = u' "936e852f-4c41-4524-8387-bd87090e9fcc"

project9 :: Project
project9 =
  project2
    { uuid = project9Uuid
    , name = "My VisibleEdit Project SharedView"
    , visibility = VisibleEditProjectVisibility
    , sharing = AnyoneWithLinkViewProjectSharing
    , permissions = [project9AlbertEditProjectPerm]
    }

project9Events :: [ProjectEvent]
project9Events = fEvents project9Uuid

project9EventsEdited :: [ProjectEvent]
project9EventsEdited = fEventsEdited project9Uuid

project9Versions :: [ProjectVersion]
project9Versions = qVersions project9Uuid

project9AlbertEditProjectPerm :: ProjectPerm
project9AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project9.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project10Uuid :: U.UUID
project10Uuid = u' "3c8e7ce6-cb5e-4cd1-a4d1-fb9de55f67ed"

project10 :: Project
project10 =
  project3
    { uuid = project10Uuid
    , name = "My VisibleEdit Project SharedEdit"
    , visibility = VisibleEditProjectVisibility
    , sharing = AnyoneWithLinkEditProjectSharing
    }

project10Events :: [ProjectEvent]
project10Events = fEvents project10Uuid

project10EventsEdited :: [ProjectEvent]
project10EventsEdited = project10Events ++ [setCreatedBy (slble_rQ2' project10Uuid) Nothing]

project10Versions :: [ProjectVersion]
project10Versions = qVersions project10Uuid

project10Ctn :: ProjectContent
project10Ctn =
  ProjectContent
    { phaseUuid = Just $ phase1.uuid
    , replies = fReplies
    , labels = fLabels
    }

project10EditedShare :: Project
project10EditedShare = project10 {permissions = [project10NikolaEditProjectPerm]}

project10EditedSettings :: Project
project10EditedSettings = project10 {name = "EDITED: " ++ project10.name}

project10NikolaEditProjectPerm :: ProjectPerm
project10NikolaEditProjectPerm =
  ProjectPerm
    { projectUuid = project10.uuid
    , memberType = UserProjectPermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project10NikolaEditProjectPermDto :: ProjectPermDTO
project10NikolaEditProjectPermDto = toUserProjectPermDTO project10NikolaEditProjectPerm userNikola

project10EditedSettingsChange :: ProjectSettingsChangeDTO
project10EditedSettingsChange =
  ProjectSettingsChangeDTO
    { name = "EDITED: " ++ project10.name
    , description = project10.description
    , projectTags = project10.projectTags
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , isTemplate = project10.isTemplate
    }

project10EditedWs :: ProjectDetailWsDTO
project10EditedWs =
  ProjectDetailWsDTO
    { name = project10EditedSettingsChange.name
    , description = project10EditedSettingsChange.description
    , visibility = project10.visibility
    , sharing = project10.sharing
    , projectTags = project10EditedSettingsChange.projectTags
    , permissions = []
    , documentTemplateId = Nothing
    , documentTemplate = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , isTemplate = project10EditedSettingsChange.isTemplate
    , labels = M.empty
    , unresolvedCommentCounts = M.empty
    , resolvedCommentCounts = M.empty
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project11Uuid :: U.UUID
project11Uuid = u' "ba6b6c0e-2bb7-40e7-9019-feb943756888"

project11 :: Project
project11 =
  project1
    { uuid = project11Uuid
    , name = "My Project from project template"
    , permissions = [project11AlbertEditProjectPerm]
    }

project11Events :: [ProjectEvent]
project11Events = fEvents project11Uuid

project11Versions :: [ProjectVersion]
project11Versions = qVersions project11Uuid

project11Ctn :: ProjectContent
project11Ctn = project1Ctn

project11Dto :: ProjectDTO
project11Dto = toSimpleDTO project11 germanyKmPackage DefaultProjectState [project11AlbertEditProjectPermDto]

project11AlbertEditProjectPerm :: ProjectPerm
project11AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project11.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project11AlbertEditProjectPermDto :: ProjectPermDTO
project11AlbertEditProjectPermDto = toUserProjectPermDTO project11AlbertEditProjectPerm userAlbert

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project12Uuid :: U.UUID
project12Uuid = u' "e02bc040-7446-48a2-b557-678e01d66937"

project12 :: Project
project12 =
  project1
    { uuid = project12Uuid
    , name = "My Private Project with 2 users"
    , visibility = VisibleEditProjectVisibility
    , sharing = AnyoneWithLinkEditProjectSharing
    , permissions = [project12NikolaEditProjectPerm, project12AlbertEditProjectPerm]
    , updatedAt = dt' 2018 1 23
    }

project12Events :: [ProjectEvent]
project12Events = fEvents project12Uuid

project12Versions :: [ProjectVersion]
project12Versions = qVersions project12Uuid

project12Ctn :: ProjectContent
project12Ctn = project1Ctn

project12Dto :: ProjectDTO
project12Dto =
  toSimpleDTO project12 germanyKmPackage DefaultProjectState [project12NikolaEditProjectPermDto, project12AlbertEditProjectPermDto]

project12AlbertEditProjectPerm :: ProjectPerm
project12AlbertEditProjectPerm =
  ProjectPerm
    { projectUuid = project12.uuid
    , memberType = UserProjectPermType
    , memberUuid = userAlbert.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project12AlbertEditProjectPermDto :: ProjectPermDTO
project12AlbertEditProjectPermDto = toUserProjectPermDTO project12AlbertEditProjectPerm userAlbert

project12NikolaEditProjectPerm :: ProjectPerm
project12NikolaEditProjectPerm =
  ProjectPerm
    { projectUuid = project12.uuid
    , memberType = UserProjectPermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project12NikolaEditProjectPermDto :: ProjectPermDTO
project12NikolaEditProjectPermDto = toUserProjectPermDTO project12NikolaEditProjectPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project13Uuid :: U.UUID
project13Uuid = u' "59b97a8e-aa48-47f7-93a7-646f9df077df"

project13 :: Project
project13 =
  project1
    { uuid = project13Uuid
    , name = "My VisibleCommentProjectVisibility Project"
    , visibility = VisibleCommentProjectVisibility
    , permissions = [project13NikolaCommentProjectPerm]
    }

project13Events :: [ProjectEvent]
project13Events = fEvents project13Uuid

project13Versions :: [ProjectVersion]
project13Versions = qVersions project13Uuid

project13Ctn :: ProjectContent
project13Ctn = project1Ctn

project13Dto :: ProjectDTO
project13Dto = toSimpleDTO project13 germanyKmPackage DefaultProjectState [project13NikolaCommentProjectPermDto]

project13NikolaCommentProjectPerm :: ProjectPerm
project13NikolaCommentProjectPerm =
  ProjectPerm
    { projectUuid = project13.uuid
    , memberType = UserProjectPermType
    , memberUuid = userNikola.uuid
    , perms = commentatorPermissions
    , tenantUuid = defaultTenant.uuid
    }

project13NikolaCommentProjectPermDto :: ProjectPermDTO
project13NikolaCommentProjectPermDto = toUserProjectPermDTO project13NikolaCommentProjectPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project14 :: Project
project14 =
  project1
    { uuid = u' "8355fe3c-47b9-4078-b5b6-08aa0188e85f"
    , name = "My different KM Project"
    , permissions = [project14NikolaEditProjectPerm]
    , knowledgeModelPackageId = amsterdamKmPackage.pId
    , updatedAt = dt' 2018 1 26
    }

project14Events :: [ProjectEvent]
project14Events = []

project14Ctn :: ProjectContent
project14Ctn = project1Ctn

project14Dto :: ProjectDTO
project14Dto = toSimpleDTO project14 amsterdamKmPackage DefaultProjectState [project14NikolaEditProjectPermDto]

project14NikolaEditProjectPerm :: ProjectPerm
project14NikolaEditProjectPerm =
  ProjectPerm
    { projectUuid = project14.uuid
    , memberType = UserProjectPermType
    , memberUuid = userNikola.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project14NikolaEditProjectPermDto :: ProjectPermDTO
project14NikolaEditProjectPermDto = toUserProjectPermDTO project14NikolaEditProjectPerm userNikola

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
project15Uuid :: U.UUID
project15Uuid = u' "d09695f4-638b-472b-9951-a31bd7dc91f7"

project15 :: Project
project15 =
  Project
    { uuid = project15Uuid
    , name = "My Group Project"
    , description = Just "Some description"
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = germanyKmPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just wizardDocumentTemplate.tId
    , formatUuid = Just formatJson.uuid
    , creatorUuid = Nothing
    , permissions = [project15GroupEditProjectPerm]
    , isTemplate = False
    , squashed = True
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 29
    }

project15AnonymousEdit :: Project
project15AnonymousEdit =
  project15
    { sharing = AnyoneWithLinkEditProjectSharing
    }

project15AnonymousComment :: Project
project15AnonymousComment =
  project15
    { sharing = AnyoneWithLinkCommentProjectSharing
    }

project15NoPerms :: Project
project15NoPerms =
  project15
    { permissions = []
    }

project15Events :: [ProjectEvent]
project15Events = fEvents project15Uuid

project15Versions :: [ProjectVersion]
project15Versions = qVersions project15Uuid

project15Dto :: ProjectDTO
project15Dto = toSimpleDTO project15 germanyKmPackage DefaultProjectState [project15GroupEditProjectPermDto]

project15GroupEditProjectPerm :: ProjectPerm
project15GroupEditProjectPerm =
  ProjectPerm
    { projectUuid = project15.uuid
    , memberType = UserGroupProjectPermType
    , memberUuid = bioGroup.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

project15GroupEditProjectPermDto :: ProjectPermDTO
project15GroupEditProjectPermDto = toUserGroupProjectPermDTO project15GroupEditProjectPerm bioGroup

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
differentProject :: Project
differentProject =
  Project
    { uuid = u' "7bf4a83e-1687-4e99-b1df-9221977d7b4f"
    , name = "My Different Project"
    , description = Just "Some description"
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = differentPackage.pId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Just $ anotherWizardDocumentTemplate.tId
    , formatUuid = Just $ formatJson.uuid
    , creatorUuid = Just $ userCharles.uuid
    , permissions = [differentCharlesOwnerProjectPerm]
    , isTemplate = True
    , squashed = True
    , tenantUuid = differentTenant.uuid
    , createdAt = dt' 2018 1 20
    , updatedAt = dt' 2018 1 25
    }

differentProjectEvents :: [ProjectEvent]
differentProjectEvents = []

differentProjectVersions :: [ProjectVersion]
differentProjectVersions = []

differentCharlesOwnerProjectPerm :: ProjectPerm
differentCharlesOwnerProjectPerm =
  ProjectPerm
    { projectUuid = differentProject.uuid
    , memberType = UserProjectPermType
    , memberUuid = userCharles.uuid
    , perms = ownerPermissions
    , tenantUuid = differentTenant.uuid
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
contentChangeDTO :: ProjectContentChangeDTO
contentChangeDTO =
  ProjectContentChangeDTO
    { events = fmap toEventChangeDTO (fEvents U.nil)
    }

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
bioGroupEditProjectPerm :: ProjectPerm
bioGroupEditProjectPerm =
  ProjectPerm
    { projectUuid = project1.uuid
    , memberType = UserGroupProjectPermType
    , memberUuid = bioGroup.uuid
    , perms = ownerPermissions
    , tenantUuid = defaultTenant.uuid
    }

bioGroupEditProjectPermDto :: ProjectPermDTO
bioGroupEditProjectPermDto = toUserGroupProjectPermDTO bioGroupEditProjectPerm bioGroup
