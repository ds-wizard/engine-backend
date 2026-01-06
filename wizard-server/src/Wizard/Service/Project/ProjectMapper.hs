module Wizard.Service.Project.ProjectMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as SPM
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Api.Resource.Project.ProjectContentDTO
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectReportDTO
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.Acl
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Comment.ProjectCommentList
import Wizard.Model.Project.Detail.ProjectDetail
import Wizard.Model.Project.Detail.ProjectDetailQuestionnaire
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectList
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Project.ProjectSimple
import Wizard.Model.Project.ProjectState
import Wizard.Model.Project.ProjectSuggestion
import Wizard.Model.Project.Version.ProjectVersionList
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Acl.AclMapper
import Wizard.Service.Project.Event.ProjectEventMapper
import WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand
import WizardLib.Public.Model.User.UserGroup

toDTO :: Project -> KnowledgeModelPackage -> ProjectState -> [ProjectPermDTO] -> ProjectDTO
toDTO project kmPackage state permissions =
  ProjectDTO
    { uuid = project.uuid
    , name = project.name
    , description = project.description
    , visibility = project.visibility
    , sharing = project.sharing
    , state = state
    , knowledgeModelPackage = SPM.toSimple kmPackage
    , permissions = permissions
    , isTemplate = project.isTemplate
    , createdAt = project.createdAt
    , updatedAt = project.updatedAt
    }

toDTO' :: ProjectList -> ProjectDTO
toDTO' project =
  ProjectDTO
    { uuid = project.uuid
    , name = project.name
    , description = project.description
    , visibility = project.visibility
    , sharing = project.sharing
    , state = project.state
    , knowledgeModelPackage = project.knowledgeModelPackage
    , permissions = project.permissions
    , isTemplate = project.isTemplate
    , createdAt = project.createdAt
    , updatedAt = project.updatedAt
    }

toSimpleDTO :: Project -> KnowledgeModelPackage -> ProjectState -> [ProjectPermDTO] -> ProjectDTO
toSimpleDTO project kmPackage state permissions =
  ProjectDTO
    { uuid = project.uuid
    , name = project.name
    , description = project.description
    , visibility = project.visibility
    , sharing = project.sharing
    , state = state
    , knowledgeModelPackage = SPM.toSimple kmPackage
    , permissions = permissions
    , isTemplate = project.isTemplate
    , createdAt = project.createdAt
    , updatedAt = project.updatedAt
    }

toDetailQuestionnaire :: Project -> Maybe U.UUID -> [ProjectPermDTO] -> Int -> Int -> ProjectDetailQuestionnaire
toDetailQuestionnaire project migrationUuid permissions projectActionsAvailable projectImportersAvailable =
  ProjectDetailQuestionnaire
    { uuid = project.uuid
    , name = project.name
    , visibility = project.visibility
    , sharing = project.sharing
    , knowledgeModelPackageId = project.knowledgeModelPackageId
    , selectedQuestionTagUuids = project.selectedQuestionTagUuids
    , isTemplate = project.isTemplate
    , migrationUuid = migrationUuid
    , permissions = permissions
    , files = []
    , projectActionsAvailable = projectActionsAvailable
    , projectImportersAvailable = projectImportersAvailable
    }

toDetailDTO :: ProjectDetail -> ProjectDetailDTO
toDetailDTO ProjectDetail {..} =
  ProjectDetailDTO {..}

toDetailProjectDTO :: ProjectDetailQuestionnaire -> M.Map String (M.Map U.UUID Int) -> M.Map String (M.Map U.UUID Int) -> KnowledgeModel -> Maybe U.UUID -> M.Map String Reply -> M.Map String [U.UUID] -> ProjectDetailQuestionnaireDTO
toDetailProjectDTO ProjectDetailQuestionnaire {..} unresolvedCommentCounts resolvedCommentCounts knowledgeModel phaseUuid replies labels =
  let fileCount = length files
   in ProjectDetailQuestionnaireDTO {..}

toDetailWsDTO :: Project -> Maybe DocumentTemplateDTO -> Maybe DocumentTemplateFormatSimple -> [ProjectPermDTO] -> M.Map String [U.UUID] -> M.Map String (M.Map U.UUID Int) -> M.Map String (M.Map U.UUID Int) -> ProjectDetailWsDTO
toDetailWsDTO project mTemplate mFormat projectPerms labels unresolvedCommentCounts resolvedCommentCounts =
  ProjectDetailWsDTO
    { name = project.name
    , description = project.description
    , visibility = project.visibility
    , sharing = project.sharing
    , projectTags = project.projectTags
    , documentTemplateId = project.documentTemplateId
    , documentTemplate = mTemplate
    , formatUuid = project.formatUuid
    , format = mFormat
    , permissions = projectPerms
    , isTemplate = project.isTemplate
    , labels = labels
    , unresolvedCommentCounts = unresolvedCommentCounts
    , resolvedCommentCounts = resolvedCommentCounts
    }

toContentDTO
  :: ProjectContent
  -> M.Map String [ProjectCommentThreadList]
  -> [ProjectEventList]
  -> [ProjectVersionList]
  -> ProjectContentDTO
toContentDTO projectContent threads events versions =
  ProjectContentDTO
    { phaseUuid = projectContent.phaseUuid
    , replies = projectContent.replies
    , commentThreadsMap = threads
    , labels = projectContent.labels
    , events = events
    , versions = versions
    }

toProjectReportDTO :: [Indication] -> ProjectReportDTO
toProjectReportDTO indications = ProjectReportDTO {indications = indications}

toChangeDTO :: Project -> ProjectShareChangeDTO
toChangeDTO project =
  ProjectShareChangeDTO
    { visibility = project.visibility
    , sharing = project.sharing
    , permissions = fmap toProjectPermChangeDTO project.permissions
    }

toUserProjectPerm :: U.UUID -> U.UUID -> [String] -> U.UUID -> ProjectPerm
toUserProjectPerm projectUuid userUuid perms tenantUuid =
  ProjectPerm
    { projectUuid = projectUuid
    , memberType = UserProjectPermType
    , memberUuid = userUuid
    , perms = perms
    , tenantUuid = tenantUuid
    }

toUserGroupProjectPerm :: U.UUID -> U.UUID -> [String] -> U.UUID -> ProjectPerm
toUserGroupProjectPerm projectUuid userGroupUuid perms tenantUuid =
  ProjectPerm
    { projectUuid = projectUuid
    , memberType = UserGroupProjectPermType
    , memberUuid = userGroupUuid
    , perms = perms
    , tenantUuid = tenantUuid
    }

toUserProjectPermDTO :: ProjectPerm -> User -> ProjectPermDTO
toUserProjectPermDTO projectPerm user =
  ProjectPermDTO
    { projectUuid = projectPerm.projectUuid
    , member = toUserMemberDTO user
    , perms = projectPerm.perms
    }

toUserGroupProjectPermDTO :: ProjectPerm -> UserGroup -> ProjectPermDTO
toUserGroupProjectPermDTO projectPerm userGroup =
  ProjectPermDTO
    { projectUuid = projectPerm.projectUuid
    , member = toUserGroupMemberDTO userGroup
    , perms = projectPerm.perms
    }

toProjectPermChangeDTO :: ProjectPerm -> ProjectPermChangeDTO
toProjectPermChangeDTO projectPerm =
  ProjectPermChangeDTO
    { memberUuid = projectPerm.memberUuid
    , memberType = projectPerm.memberType
    , perms = projectPerm.perms
    }

toSimple :: Project -> ProjectSimple
toSimple project = ProjectSimple {uuid = project.uuid, name = project.name}

toSuggestion :: Project -> ProjectSuggestion
toSuggestion project = ProjectSuggestion {uuid = project.uuid, name = project.name, description = project.description}

toCreateFromTemplateDTO :: Project -> ProjectCreateFromTemplateDTO
toCreateFromTemplateDTO project =
  ProjectCreateFromTemplateDTO
    { name = project.name
    , projectUuid = project.uuid
    }

toProjectDetailTemplateState :: Maybe DocumentTemplate -> Maybe DocumentTemplateState
toProjectDetailTemplateState =
  fmap
    ( \tml ->
        if isDocumentTemplateUnsupported tml.metamodelVersion
          then UnsupportedMetamodelVersionDocumentTemplateState
          else DefaultDocumentTemplateState
    )

fromShareChangeDTO :: Project -> ProjectShareChangeDTO -> ProjectVisibility -> ProjectSharing -> UTCTime -> Project
fromShareChangeDTO project dto visibility sharing now =
  Project
    { uuid = project.uuid
    , name = project.name
    , description = project.description
    , visibility = visibility
    , sharing = sharing
    , knowledgeModelPackageId = project.knowledgeModelPackageId
    , selectedQuestionTagUuids = project.selectedQuestionTagUuids
    , projectTags = project.projectTags
    , documentTemplateId = project.documentTemplateId
    , formatUuid = project.formatUuid
    , creatorUuid = project.creatorUuid
    , permissions = fmap (fromProjectPermChangeDTO project.uuid project.tenantUuid) dto.permissions
    , isTemplate = project.isTemplate
    , squashed = project.squashed
    , tenantUuid = project.tenantUuid
    , createdAt = project.createdAt
    , updatedAt = now
    }

fromSettingsChangeDTO :: Project -> ProjectSettingsChangeDTO -> UserDTO -> UTCTime -> Project
fromSettingsChangeDTO project dto currentUser now =
  Project
    { uuid = project.uuid
    , name = dto.name
    , description = dto.description
    , visibility = project.visibility
    , sharing = project.sharing
    , knowledgeModelPackageId = project.knowledgeModelPackageId
    , selectedQuestionTagUuids = project.selectedQuestionTagUuids
    , projectTags = dto.projectTags
    , documentTemplateId = dto.documentTemplateId
    , formatUuid = dto.formatUuid
    , creatorUuid = project.creatorUuid
    , permissions = project.permissions
    , isTemplate =
        if _PRJ_TML_PERM `elem` currentUser.permissions
          then dto.isTemplate
          else project.isTemplate
    , squashed = project.squashed
    , tenantUuid = project.tenantUuid
    , createdAt = project.createdAt
    , updatedAt = now
    }

fromProjectCreateDTO
  :: ProjectCreateDTO
  -> U.UUID
  -> ProjectVisibility
  -> ProjectSharing
  -> Maybe U.UUID
  -> String
  -> U.UUID
  -> Maybe U.UUID
  -> U.UUID
  -> UTCTime
  -> (Project, [ProjectEvent])
fromProjectCreateDTO dto projectUuid visibility sharing mCurrentUserUuid pkgId phaseEventUuid mPhase tenantUuid now =
  ( Project
      { uuid = projectUuid
      , name = dto.name
      , description = Nothing
      , visibility = visibility
      , sharing = sharing
      , knowledgeModelPackageId = pkgId
      , selectedQuestionTagUuids = dto.questionTagUuids
      , projectTags = []
      , documentTemplateId = dto.documentTemplateId
      , formatUuid = dto.formatUuid
      , creatorUuid = mCurrentUserUuid
      , permissions =
          case mCurrentUserUuid of
            Just currentUserUuid -> [toUserProjectPerm projectUuid currentUserUuid ownerPermissions tenantUuid]
            Nothing -> []
      , isTemplate = False
      , squashed = True
      , tenantUuid = tenantUuid
      , createdAt = now
      , updatedAt = now
      }
  , case mPhase of
      Just phase ->
        [ SetPhaseEvent' $
            SetPhaseEvent
              { uuid = phaseEventUuid
              , phaseUuid = Just phase
              , projectUuid = projectUuid
              , tenantUuid = tenantUuid
              , createdBy = mCurrentUserUuid
              , createdAt = now
              }
        ]
      Nothing -> []
  )

fromContentChangeDTO :: Project -> [ProjectEvent] -> ProjectContentChangeDTO -> Maybe UserDTO -> UTCTime -> (Project, [ProjectEvent])
fromContentChangeDTO project events dto mCurrentUser now =
  let newTodoEvents = fmap (\e -> fromEventChangeDTO e project.uuid project.tenantUuid (fmap (.uuid) mCurrentUser) now) dto.events
      updatedEvents = events ++ newTodoEvents
   in (project {updatedAt = now}, updatedEvents)

fromProjectPermChangeDTO :: U.UUID -> U.UUID -> ProjectPermChangeDTO -> ProjectPerm
fromProjectPermChangeDTO projectUuid tenantUuid dto =
  ProjectPerm
    { projectUuid = projectUuid
    , memberType = dto.memberType
    , memberUuid = dto.memberUuid
    , perms = dto.perms
    , tenantUuid = tenantUuid
    }

fromCreateProjectCommand :: CreateProjectCommand -> U.UUID -> [ProjectPerm] -> TenantConfigProject -> U.UUID -> UTCTime -> Project
fromCreateProjectCommand command uuid permissions tcProject createdBy now = do
  Project
    { uuid = uuid
    , name = command.name
    , description = Nothing
    , visibility = tcProject.projectVisibility.defaultValue
    , sharing = tcProject.projectSharing.defaultValue
    , knowledgeModelPackageId = command.knowledgeModelPackageId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = command.documentTemplateId
    , formatUuid = Nothing
    , creatorUuid = Just createdBy
    , permissions = permissions
    , isTemplate = False
    , squashed = True
    , tenantUuid = tcProject.tenantUuid
    , createdAt = now
    , updatedAt = now
    }
