module Wizard.Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Template.Template
import qualified Shared.Service.Package.PackageMapper as SPM
import qualified Shared.Service.Template.TemplateMapper as STM
import Shared.Service.Template.TemplateMapper
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Constant.Acl
import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Service.Acl.AclMapper
import qualified Wizard.Service.Package.PackageMapper as PM
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

toDTO ::
     Questionnaire
  -> QuestionnaireContent
  -> Package
  -> QuestionnaireState
  -> QuestionnaireReportDTO
  -> [QuestionnairePermRecordDTO]
  -> QuestionnaireDTO
toDTO qtn qtnCtn package state report permissions =
  QuestionnaireDTO
    { _questionnaireDTOUuid = qtn ^. uuid
    , _questionnaireDTOName = qtn ^. name
    , _questionnaireDTODescription = qtn ^. description
    , _questionnaireDTOPhaseUuid = qtnCtn ^. phaseUuid
    , _questionnaireDTOVisibility = qtn ^. visibility
    , _questionnaireDTOSharing = qtn ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = SPM.toSimple package
    , _questionnaireDTOReport = report
    , _questionnaireDTOPermissions = permissions
    , _questionnaireDTOIsTemplate = qtn ^. isTemplate
    , _questionnaireDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDTOUpdatedAt = qtn ^. updatedAt
    }

toDTO' :: QuestionnaireDetail -> QuestionnaireContent -> QuestionnaireReportDTO -> QuestionnaireDTO
toDTO' qtn qtnCtn report =
  QuestionnaireDTO
    { _questionnaireDTOUuid = qtn ^. uuid
    , _questionnaireDTOName = qtn ^. name
    , _questionnaireDTODescription = qtn ^. description
    , _questionnaireDTOPhaseUuid = qtnCtn ^. phaseUuid
    , _questionnaireDTOVisibility = qtn ^. visibility
    , _questionnaireDTOSharing = qtn ^. sharing
    , _questionnaireDTOState = qtn ^. state
    , _questionnaireDTOPackage = qtn ^. package
    , _questionnaireDTOReport = report
    , _questionnaireDTOPermissions = qtn ^. permissions
    , _questionnaireDTOIsTemplate = qtn ^. isTemplate
    , _questionnaireDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDTOUpdatedAt = qtn ^. updatedAt
    }

toSimpleDTO ::
     Questionnaire
  -> QuestionnaireContent
  -> PackageWithEvents
  -> QuestionnaireState
  -> QuestionnaireReportDTO
  -> [QuestionnairePermRecordDTO]
  -> QuestionnaireDTO
toSimpleDTO qtn qtnCtn package state report permissions =
  QuestionnaireDTO
    { _questionnaireDTOUuid = qtn ^. uuid
    , _questionnaireDTOName = qtn ^. name
    , _questionnaireDTODescription = qtn ^. description
    , _questionnaireDTOPhaseUuid = qtnCtn ^. phaseUuid
    , _questionnaireDTOVisibility = qtn ^. visibility
    , _questionnaireDTOSharing = qtn ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = SPM.toSimple . SPM.toPackage $ package
    , _questionnaireDTOReport = report
    , _questionnaireDTOPermissions = permissions
    , _questionnaireDTOIsTemplate = qtn ^. isTemplate
    , _questionnaireDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDTOUpdatedAt = qtn ^. updatedAt
    }

toDetailWithPackageWithEventsDTO ::
     Questionnaire
  -> QuestionnaireContent
  -> Package
  -> [String]
  -> KnowledgeModel
  -> QuestionnaireState
  -> Maybe Template
  -> Maybe TemplateFormat
  -> M.Map String Reply
  -> M.Map String [QuestionnaireCommentThread]
  -> [QuestionnairePermRecordDTO]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO qtn qtnCtn pkg pkgVersions knowledgeModel state mTemplate mFormat replies threads records events versions =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = qtn ^. uuid
    , _questionnaireDetailDTOName = qtn ^. name
    , _questionnaireDetailDTODescription = qtn ^. description
    , _questionnaireDetailDTOPhaseUuid = qtnCtn ^. phaseUuid
    , _questionnaireDetailDTOVisibility = qtn ^. visibility
    , _questionnaireDetailDTOSharing = qtn ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = PM.toSimpleDTO' [] [] pkgVersions pkg
    , _questionnaireDetailDTOSelectedQuestionTagUuids = qtn ^. selectedQuestionTagUuids
    , _questionnaireDetailDTOProjectTags = qtn ^. projectTags
    , _questionnaireDetailDTOTemplateId = qtn ^. templateId
    , _questionnaireDetailDTOTemplate = fmap STM.toDTO mTemplate
    , _questionnaireDetailDTOFormatUuid = qtn ^. formatUuid
    , _questionnaireDetailDTOFormat = fmap toFormatDTO mFormat
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = replies
    , _questionnaireDetailDTOCommentThreadsMap = threads
    , _questionnaireDetailDTOLabels = qtnCtn ^. labels
    , _questionnaireDetailDTOPermissions = records
    , _questionnaireDetailDTOEvents = events
    , _questionnaireDetailDTOVersions = versions
    , _questionnaireDetailDTOCreatorUuid = qtn ^. creatorUuid
    , _questionnaireDetailDTOIsTemplate = qtn ^. isTemplate
    , _questionnaireDetailDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = qtn ^. updatedAt
    }

toDetailWithPackageDTO ::
     Questionnaire
  -> QuestionnaireContent
  -> PackageSimpleDTO
  -> KnowledgeModel
  -> QuestionnaireState
  -> Maybe Template
  -> Maybe TemplateFormat
  -> M.Map String Reply
  -> M.Map String [QuestionnaireCommentThread]
  -> [QuestionnairePermRecordDTO]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireDetailDTO
toDetailWithPackageDTO qtn qtnContent package knowledgeModel state mTemplate mFormat replies threads records events versions =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = qtn ^. uuid
    , _questionnaireDetailDTOName = qtn ^. name
    , _questionnaireDetailDTODescription = qtn ^. description
    , _questionnaireDetailDTOPhaseUuid = qtnContent ^. phaseUuid
    , _questionnaireDetailDTOVisibility = qtn ^. visibility
    , _questionnaireDetailDTOSharing = qtn ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = package
    , _questionnaireDetailDTOSelectedQuestionTagUuids = qtn ^. selectedQuestionTagUuids
    , _questionnaireDetailDTOProjectTags = qtn ^. projectTags
    , _questionnaireDetailDTOTemplateId = qtn ^. templateId
    , _questionnaireDetailDTOTemplate = fmap STM.toDTO mTemplate
    , _questionnaireDetailDTOFormatUuid = qtn ^. formatUuid
    , _questionnaireDetailDTOFormat = fmap toFormatDTO mFormat
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = replies
    , _questionnaireDetailDTOCommentThreadsMap = threads
    , _questionnaireDetailDTOLabels = qtnContent ^. labels
    , _questionnaireDetailDTOPermissions = records
    , _questionnaireDetailDTOEvents = events
    , _questionnaireDetailDTOVersions = versions
    , _questionnaireDetailDTOCreatorUuid = qtn ^. creatorUuid
    , _questionnaireDetailDTOIsTemplate = qtn ^. isTemplate
    , _questionnaireDetailDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = qtn ^. updatedAt
    }

toContentDTO ::
     QuestionnaireContent
  -> M.Map String [QuestionnaireCommentThread]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireContentDTO
toContentDTO qtnCtn threads events versions =
  QuestionnaireContentDTO
    { _questionnaireContentDTOPhaseUuid = qtnCtn ^. phaseUuid
    , _questionnaireContentDTOReplies = qtnCtn ^. replies
    , _questionnaireContentDTOCommentThreadsMap = threads
    , _questionnaireContentDTOLabels = qtnCtn ^. labels
    , _questionnaireContentDTOEvents = events
    , _questionnaireContentDTOVersions = versions
    }

toQuestionnaireReportDTO :: [Indication] -> QuestionnaireReportDTO
toQuestionnaireReportDTO indications = QuestionnaireReportDTO {_questionnaireReportDTOIndications = indications}

toChangeDTO :: Questionnaire -> QuestionnaireChangeDTO
toChangeDTO qtn =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = qtn ^. name
    , _questionnaireChangeDTODescription = qtn ^. description
    , _questionnaireChangeDTOVisibility = qtn ^. visibility
    , _questionnaireChangeDTOSharing = qtn ^. sharing
    , _questionnaireChangeDTOProjectTags = qtn ^. projectTags
    , _questionnaireChangeDTOPermissions = qtn ^. permissions
    , _questionnaireChangeDTOTemplateId = qtn ^. templateId
    , _questionnaireChangeDTOFormatUuid = qtn ^. formatUuid
    , _questionnaireChangeDTOIsTemplate = qtn ^. isTemplate
    }

toUserPermRecord :: U.UUID -> U.UUID -> U.UUID -> [String] -> QuestionnairePermRecord
toUserPermRecord permUuid questionnaireUuid userUuid perms =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = permUuid
    , _questionnairePermRecordQuestionnaireUuid = questionnaireUuid
    , _questionnairePermRecordMember = UserMember {_userMemberUuid = userUuid}
    , _questionnairePermRecordPerms = perms
    }

toGroupPermRecord :: U.UUID -> U.UUID -> String -> [String] -> QuestionnairePermRecord
toGroupPermRecord permUuid questionnaireUuid groupId perms =
  QuestionnairePermRecord
    { _questionnairePermRecordUuid = permUuid
    , _questionnairePermRecordQuestionnaireUuid = questionnaireUuid
    , _questionnairePermRecordMember = GroupMember {_groupMemberGId = groupId}
    , _questionnairePermRecordPerms = perms
    }

toUserPermRecordDTO :: QuestionnairePermRecord -> User -> QuestionnairePermRecordDTO
toUserPermRecordDTO record user =
  QuestionnairePermRecordDTO
    { _questionnairePermRecordDTOUuid = record ^. uuid
    , _questionnairePermRecordDTOQuestionnaireUuid = record ^. questionnaireUuid
    , _questionnairePermRecordDTOMember = toUserMemberDTO user
    , _questionnairePermRecordDTOPerms = record ^. perms
    }

toGroupPermRecordDTO :: QuestionnairePermRecord -> Group -> QuestionnairePermRecordDTO
toGroupPermRecordDTO record group =
  QuestionnairePermRecordDTO
    { _questionnairePermRecordDTOUuid = record ^. uuid
    , _questionnairePermRecordDTOQuestionnaireUuid = record ^. questionnaireUuid
    , _questionnairePermRecordDTOMember = toGroupMemberDTO group
    , _questionnairePermRecordDTOPerms = record ^. perms
    }

toSimple :: Questionnaire -> QuestionnaireSimple
toSimple qtn = QuestionnaireSimple {_questionnaireSimpleUuid = qtn ^. uuid, _questionnaireSimpleName = qtn ^. name}

toCreateFromTemplateDTO :: Questionnaire -> QuestionnaireCreateFromTemplateDTO
toCreateFromTemplateDTO qtn =
  QuestionnaireCreateFromTemplateDTO
    { _questionnaireCreateFromTemplateDTOName = qtn ^. name
    , _questionnaireCreateFromTemplateDTOQuestionnaireUuid = qtn ^. uuid
    }

fromChangeDTO ::
     Questionnaire
  -> QuestionnaireChangeDTO
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> UserDTO
  -> UTCTime
  -> Questionnaire
fromChangeDTO qtn dto visibility sharing currentUser now =
  Questionnaire
    { _questionnaireUuid = qtn ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireDescription = dto ^. description
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = qtn ^. packageId
    , _questionnaireSelectedQuestionTagUuids = qtn ^. selectedQuestionTagUuids
    , _questionnaireProjectTags = dto ^. projectTags
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireCreatorUuid = qtn ^. creatorUuid
    , _questionnairePermissions = fmap sanitizePerms (dto ^. permissions)
    , _questionnaireEvents = qtn ^. events
    , _questionnaireVersions = qtn ^. versions
    , _questionnaireIsTemplate =
        if _QTN_TML_PERM `elem` (currentUser ^. permissions)
          then dto ^. isTemplate
          else qtn ^. isTemplate
    , _questionnaireSquashed = qtn ^. squashed
    , _questionnaireAppUuid = qtn ^. appUuid
    , _questionnaireCreatedAt = qtn ^. createdAt
    , _questionnaireUpdatedAt = now
    }
  where
    sanitizePerms perm = perm & questionnaireUuid .~ (qtn ^. uuid)

fromQuestionnaireCreateDTO ::
     QuestionnaireCreateDTO
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
fromQuestionnaireCreateDTO dto qtnUuid visibility sharing mCurrentUserUuid pkgId phaseEventUuid mPhase appUuid now permUuid =
  Questionnaire
    { _questionnaireUuid = qtnUuid
    , _questionnaireName = dto ^. name
    , _questionnaireDescription = Nothing
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = pkgId
    , _questionnaireSelectedQuestionTagUuids = dto ^. questionTagUuids
    , _questionnaireProjectTags = []
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireCreatorUuid = mCurrentUserUuid
    , _questionnairePermissions =
        case mCurrentUserUuid of
          Just currentUserUuid -> [toUserPermRecord permUuid qtnUuid currentUserUuid ownerPermissions]
          Nothing -> []
    , _questionnaireEvents =
        case mPhase of
          Just phase ->
            [ SetPhaseEvent' $
              SetPhaseEvent
                { _setPhaseEventUuid = phaseEventUuid
                , _setPhaseEventPhaseUuid = Just phase
                , _setPhaseEventCreatedBy = mCurrentUserUuid
                , _setPhaseEventCreatedAt = now
                }
            ]
          Nothing -> []
    , _questionnaireVersions = []
    , _questionnaireIsTemplate = False
    , _questionnaireSquashed = True
    , _questionnaireAppUuid = appUuid
    , _questionnaireCreatedAt = now
    , _questionnaireUpdatedAt = now
    }

fromContentChangeDTO :: Questionnaire -> QuestionnaireContentChangeDTO -> Maybe UserDTO -> UTCTime -> Questionnaire
fromContentChangeDTO qtn dto mCurrentUser now =
  let newTodoEvents = fmap (\e -> fromEventChangeDTO e (mCurrentUser ^? _Just . uuid) now) (dto ^. events)
      updatedEvents = qtn ^. events ++ newTodoEvents
   in qtn {_questionnaireEvents = updatedEvents, _questionnaireUpdatedAt = now}
