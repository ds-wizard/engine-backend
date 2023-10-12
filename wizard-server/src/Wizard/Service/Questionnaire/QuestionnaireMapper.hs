module Wizard.Service.Questionnaire.QuestionnaireMapper where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.Acl
import Wizard.Model.Acl.Acl
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import Wizard.Model.Report.Report
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Acl.AclMapper
import qualified Wizard.Service.Package.PackageMapper as PM
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import qualified WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as STM
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

toDTO :: Questionnaire -> Package -> QuestionnaireState -> [QuestionnairePermRecordDTO] -> QuestionnaireDTO
toDTO qtn package state permissions =
  QuestionnaireDTO
    { uuid = qtn.uuid
    , name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , state = state
    , package = SPM.toSimple package
    , answeredQuestions = qtn.answeredQuestions
    , unansweredQuestions = qtn.unansweredQuestions
    , permissions = permissions
    , isTemplate = qtn.isTemplate
    , createdAt = qtn.createdAt
    , updatedAt = qtn.updatedAt
    }

toDTO' :: QuestionnaireDetail -> QuestionnaireDTO
toDTO' qtn =
  QuestionnaireDTO
    { uuid = qtn.uuid
    , name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , state = qtn.state
    , package = qtn.package
    , answeredQuestions = qtn.answeredQuestions
    , unansweredQuestions = qtn.unansweredQuestions
    , permissions = qtn.permissions
    , isTemplate = qtn.isTemplate
    , createdAt = qtn.createdAt
    , updatedAt = qtn.updatedAt
    }

toSimpleDTO
  :: Questionnaire -> PackageWithEvents -> QuestionnaireState -> [QuestionnairePermRecordDTO] -> QuestionnaireDTO
toSimpleDTO qtn package state permissions =
  QuestionnaireDTO
    { uuid = qtn.uuid
    , name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , state = state
    , package = SPM.toSimple . SPM.toPackage $ package
    , answeredQuestions = qtn.answeredQuestions
    , unansweredQuestions = qtn.unansweredQuestions
    , permissions = permissions
    , isTemplate = qtn.isTemplate
    , createdAt = qtn.createdAt
    , updatedAt = qtn.updatedAt
    }

toDetailWithPackageWithEventsDTO
  :: Questionnaire
  -> QuestionnaireContent
  -> Package
  -> KnowledgeModel
  -> QuestionnaireState
  -> Maybe DocumentTemplate
  -> Maybe DocumentTemplateFormat
  -> M.Map String Reply
  -> M.Map String [QuestionnaireCommentThreadDTO]
  -> [QuestionnairePermRecordDTO]
  -> [QuestionnaireVersionDTO]
  -> Maybe U.UUID
  -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO qtn qtnCtn pkg knowledgeModel state mTemplate mFormat replies threads records versions mMigrationUuid =
  QuestionnaireDetailDTO
    { uuid = qtn.uuid
    , name = qtn.name
    , description = qtn.description
    , phaseUuid = qtnCtn.phaseUuid
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , state = state
    , package = PM.toSimpleDTO' [] [] pkg
    , selectedQuestionTagUuids = qtn.selectedQuestionTagUuids
    , projectTags = qtn.projectTags
    , documentTemplateId = qtn.documentTemplateId
    , documentTemplate = fmap STM.toDTO mTemplate
    , formatUuid = qtn.formatUuid
    , format = fmap toFormatDTO mFormat
    , documentTemplateState = toQuestionnaireDetailTemplateState mTemplate
    , documentTemplatePhase = fmap (.phase) mTemplate
    , knowledgeModel = knowledgeModel
    , replies = replies
    , commentThreadsMap = threads
    , labels = qtnCtn.labels
    , permissions = records
    , versions = versions
    , creatorUuid = qtn.creatorUuid
    , isTemplate = qtn.isTemplate
    , migrationUuid = mMigrationUuid
    , createdAt = qtn.createdAt
    , updatedAt = qtn.updatedAt
    }

toDetailWsDTO :: Questionnaire -> Maybe DocumentTemplate -> Maybe DocumentTemplateFormat -> [QuestionnairePermRecordDTO] -> QuestionnaireDetailWsDTO
toDetailWsDTO qtn mTemplate mFormat records =
  QuestionnaireDetailWsDTO
    { name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , projectTags = qtn.projectTags
    , documentTemplateId = qtn.documentTemplateId
    , documentTemplate = fmap STM.toDTO mTemplate
    , formatUuid = qtn.formatUuid
    , format = fmap toFormatDTO mFormat
    , permissions = records
    , isTemplate = qtn.isTemplate
    }

toContentDTO
  :: QuestionnaireContent
  -> M.Map String [QuestionnaireCommentThreadDTO]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireContentDTO
toContentDTO qtnCtn threads events versions =
  QuestionnaireContentDTO
    { phaseUuid = qtnCtn.phaseUuid
    , replies = qtnCtn.replies
    , commentThreadsMap = threads
    , labels = qtnCtn.labels
    , events = events
    , versions = versions
    }

toQuestionnaireReportDTO :: [Indication] -> QuestionnaireReportDTO
toQuestionnaireReportDTO indications = QuestionnaireReportDTO {indications = indications}

toChangeDTO :: Questionnaire -> QuestionnaireChangeDTO
toChangeDTO qtn =
  QuestionnaireChangeDTO
    { name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , projectTags = qtn.projectTags
    , permissions = qtn.permissions
    , documentTemplateId = qtn.documentTemplateId
    , formatUuid = qtn.formatUuid
    , isTemplate = qtn.isTemplate
    }

toUserPermRecord :: U.UUID -> U.UUID -> U.UUID -> [String] -> QuestionnairePermRecord
toUserPermRecord permUuid questionnaireUuid userUuid perms =
  QuestionnairePermRecord
    { uuid = permUuid
    , questionnaireUuid = questionnaireUuid
    , member = UserMember {uuid = userUuid}
    , perms = perms
    }

toGroupPermRecord :: U.UUID -> U.UUID -> String -> [String] -> QuestionnairePermRecord
toGroupPermRecord permUuid questionnaireUuid groupId perms =
  QuestionnairePermRecord
    { uuid = permUuid
    , questionnaireUuid = questionnaireUuid
    , member = GroupMember {gId = groupId}
    , perms = perms
    }

toUserPermRecordDTO :: QuestionnairePermRecord -> User -> QuestionnairePermRecordDTO
toUserPermRecordDTO record user =
  QuestionnairePermRecordDTO
    { uuid = record.uuid
    , questionnaireUuid = record.questionnaireUuid
    , member = toUserMemberDTO user
    , perms = record.perms
    }

toGroupPermRecordDTO :: QuestionnairePermRecord -> Group -> QuestionnairePermRecordDTO
toGroupPermRecordDTO record group =
  QuestionnairePermRecordDTO
    { uuid = record.uuid
    , questionnaireUuid = record.questionnaireUuid
    , member = toGroupMemberDTO group
    , perms = record.perms
    }

toSimple :: Questionnaire -> QuestionnaireSimple
toSimple qtn = QuestionnaireSimple {uuid = qtn.uuid, name = qtn.name}

toSuggestion :: Questionnaire -> QuestionnaireSuggestion
toSuggestion qtn = QuestionnaireSuggestion {uuid = qtn.uuid, name = qtn.name, description = qtn.description}

toCreateFromTemplateDTO :: Questionnaire -> QuestionnaireCreateFromTemplateDTO
toCreateFromTemplateDTO qtn =
  QuestionnaireCreateFromTemplateDTO
    { name = qtn.name
    , questionnaireUuid = qtn.uuid
    }

toQuestionnaireDetailTemplateState :: Maybe DocumentTemplate -> Maybe DocumentTemplateState
toQuestionnaireDetailTemplateState =
  fmap
    ( \tml ->
        if tml.metamodelVersion /= documentTemplateMetamodelVersion
          then UnsupportedMetamodelVersionDocumentTemplateState
          else UnknownDocumentTemplateState
    )

fromChangeDTO
  :: Questionnaire
  -> QuestionnaireChangeDTO
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> UserDTO
  -> UTCTime
  -> Questionnaire
fromChangeDTO qtn dto visibility sharing currentUser now =
  Questionnaire
    { uuid = qtn.uuid
    , name = dto.name
    , description = dto.description
    , visibility = visibility
    , sharing = sharing
    , packageId = qtn.packageId
    , selectedQuestionTagUuids = qtn.selectedQuestionTagUuids
    , projectTags = dto.projectTags
    , documentTemplateId = dto.documentTemplateId
    , formatUuid = dto.formatUuid
    , creatorUuid = qtn.creatorUuid
    , permissions = fmap sanitizePerms dto.permissions
    , events = qtn.events
    , versions = qtn.versions
    , isTemplate =
        if _QTN_TML_PERM `elem` currentUser.permissions
          then dto.isTemplate
          else qtn.isTemplate
    , squashed = qtn.squashed
    , answeredQuestions = qtn.answeredQuestions
    , unansweredQuestions = qtn.unansweredQuestions
    , tenantUuid = qtn.tenantUuid
    , createdAt = qtn.createdAt
    , updatedAt = now
    }
  where
    sanitizePerms perm = perm {questionnaireUuid = qtn.uuid} :: QuestionnairePermRecord

fromQuestionnaireCreateDTO
  :: QuestionnaireCreateDTO
  -> U.UUID
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> Maybe U.UUID
  -> String
  -> U.UUID
  -> Maybe U.UUID
  -> U.UUID
  -> UTCTime
  -> U.UUID
  -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid visibility sharing mCurrentUserUuid pkgId phaseEventUuid mPhase tenantUuid now permUuid =
  Questionnaire
    { uuid = qtnUuid
    , name = dto.name
    , description = Nothing
    , visibility = visibility
    , sharing = sharing
    , packageId = pkgId
    , selectedQuestionTagUuids = dto.questionTagUuids
    , projectTags = []
    , documentTemplateId = dto.documentTemplateId
    , formatUuid = dto.formatUuid
    , creatorUuid = mCurrentUserUuid
    , permissions =
        case mCurrentUserUuid of
          Just currentUserUuid -> [toUserPermRecord permUuid qtnUuid currentUserUuid ownerPermissions]
          Nothing -> []
    , events =
        case mPhase of
          Just phase ->
            [ SetPhaseEvent' $
                SetPhaseEvent
                  { uuid = phaseEventUuid
                  , phaseUuid = Just phase
                  , createdBy = mCurrentUserUuid
                  , createdAt = now
                  }
            ]
          Nothing -> []
    , versions = []
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 0
    , unansweredQuestions = 0
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromContentChangeDTO :: Questionnaire -> QuestionnaireContentChangeDTO -> Maybe UserDTO -> UTCTime -> Questionnaire
fromContentChangeDTO qtn dto mCurrentUser now =
  let newTodoEvents = fmap (\e -> fromEventChangeDTO e (fmap (.uuid) mCurrentUser) now) dto.events
      updatedEvents = qtn.events ++ newTodoEvents
   in qtn {events = updatedEvents, updatedAt = now}

fromCreateQuestionnaireCommand :: CreateQuestionnaireCommand -> U.UUID -> [QuestionnairePermRecord] -> TenantConfig -> UTCTime -> Questionnaire
fromCreateQuestionnaireCommand command uuid permissions tenantConfig now = do
  Questionnaire
    { uuid = uuid
    , name = command.name
    , description = Nothing
    , visibility = tenantConfig.questionnaire.questionnaireVisibility.defaultValue
    , sharing = tenantConfig.questionnaire.questionnaireSharing.defaultValue
    , packageId = command.packageId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = command.documentTemplateId
    , formatUuid = Nothing
    , creatorUuid = Nothing
    , permissions = permissions
    , events = []
    , versions = []
    , isTemplate = False
    , squashed = True
    , answeredQuestions = 0
    , unansweredQuestions = 0
    , tenantUuid = tenantConfig.uuid
    , createdAt = now
    , updatedAt = now
    }
