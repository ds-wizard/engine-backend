module Wizard.Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
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
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Acl.Acl
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireReply
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
    , _questionnaireDTOLevel = qtnCtn ^. level
    , _questionnaireDTOVisibility = qtn ^. visibility
    , _questionnaireDTOSharing = qtn ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO package
    , _questionnaireDTOReport = report
    , _questionnaireDTOPermissions = permissions
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
    , _questionnaireDTOLevel = qtnCtn ^. level
    , _questionnaireDTOVisibility = qtn ^. visibility
    , _questionnaireDTOSharing = qtn ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO . SPM.toPackage $ package
    , _questionnaireDTOReport = report
    , _questionnaireDTOPermissions = permissions
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
  -> [QuestionnairePermRecordDTO]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO qtn qtnCtn pkg pkgVersions knowledgeModel state mTemplate mFormat replies records events versions =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = qtn ^. uuid
    , _questionnaireDetailDTOName = qtn ^. name
    , _questionnaireDetailDTOLevel = qtnCtn ^. level
    , _questionnaireDetailDTOVisibility = qtn ^. visibility
    , _questionnaireDetailDTOSharing = qtn ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = PM.toSimpleDTO' [] [] pkgVersions pkg
    , _questionnaireDetailDTOSelectedTagUuids = qtn ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateId = qtn ^. templateId
    , _questionnaireDetailDTOTemplate = fmap STM.toDTO mTemplate
    , _questionnaireDetailDTOFormatUuid = qtn ^. formatUuid
    , _questionnaireDetailDTOFormat = mFormat
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = replies
    , _questionnaireDetailDTOLabels = qtnCtn ^. labels
    , _questionnaireDetailDTOPermissions = records
    , _questionnaireDetailDTOEvents = events
    , _questionnaireDetailDTOVersions = versions
    , _questionnaireDetailDTOCreatorUuid = qtn ^. creatorUuid
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
  -> [QuestionnairePermRecordDTO]
  -> [QuestionnaireEventDTO]
  -> [QuestionnaireVersionDTO]
  -> QuestionnaireDetailDTO
toDetailWithPackageDTO qtn qtnContent package knowledgeModel state mTemplate mFormat replies records events versions =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = qtn ^. uuid
    , _questionnaireDetailDTOName = qtn ^. name
    , _questionnaireDetailDTOLevel = qtnContent ^. level
    , _questionnaireDetailDTOVisibility = qtn ^. visibility
    , _questionnaireDetailDTOSharing = qtn ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = package
    , _questionnaireDetailDTOSelectedTagUuids = qtn ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateId = qtn ^. templateId
    , _questionnaireDetailDTOTemplate = fmap STM.toDTO mTemplate
    , _questionnaireDetailDTOFormatUuid = qtn ^. formatUuid
    , _questionnaireDetailDTOFormat = mFormat
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = replies
    , _questionnaireDetailDTOLabels = qtnContent ^. labels
    , _questionnaireDetailDTOPermissions = records
    , _questionnaireDetailDTOEvents = events
    , _questionnaireDetailDTOVersions = versions
    , _questionnaireDetailDTOCreatorUuid = qtn ^. creatorUuid
    , _questionnaireDetailDTOCreatedAt = qtn ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = qtn ^. updatedAt
    }

toContentDTO :: QuestionnaireContent -> [QuestionnaireEventDTO] -> [QuestionnaireVersionDTO] -> QuestionnaireContentDTO
toContentDTO qtnCtn events versions =
  QuestionnaireContentDTO
    { _questionnaireContentDTOLevel = qtnCtn ^. level
    , _questionnaireContentDTOReplies = qtnCtn ^. replies
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
    , _questionnaireChangeDTOVisibility = qtn ^. visibility
    , _questionnaireChangeDTOSharing = qtn ^. sharing
    , _questionnaireChangeDTOPermissions = qtn ^. permissions
    , _questionnaireChangeDTOTemplateId = qtn ^. templateId
    , _questionnaireChangeDTOFormatUuid = qtn ^. formatUuid
    }

toUserPermRecord :: U.UUID -> [String] -> QuestionnairePermRecord
toUserPermRecord userUuid perms =
  QuestionnairePermRecord
    {_questionnairePermRecordMember = UserMember {_userMemberUuid = userUuid}, _questionnairePermRecordPerms = perms}

toGroupPermRecord :: String -> [String] -> QuestionnairePermRecord
toGroupPermRecord groupId perms =
  QuestionnairePermRecord
    {_questionnairePermRecordMember = GroupMember {_groupMemberGId = groupId}, _questionnairePermRecordPerms = perms}

toUserPermRecordDTO :: QuestionnairePermRecord -> User -> QuestionnairePermRecordDTO
toUserPermRecordDTO record user =
  QuestionnairePermRecordDTO
    {_questionnairePermRecordDTOMember = toUserMemberDTO user, _questionnairePermRecordDTOPerms = record ^. perms}

toGroupPermRecordDTO :: QuestionnairePermRecord -> Group -> QuestionnairePermRecordDTO
toGroupPermRecordDTO record group =
  QuestionnairePermRecordDTO
    {_questionnairePermRecordDTOMember = toGroupMemberDTO group, _questionnairePermRecordDTOPerms = record ^. perms}

fromChangeDTO ::
     Questionnaire
  -> QuestionnaireChangeDTO
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> U.UUID
  -> UTCTime
  -> Questionnaire
fromChangeDTO qtn dto visibility sharing currentUserUuid now =
  Questionnaire
    { _questionnaireUuid = qtn ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = qtn ^. packageId
    , _questionnaireSelectedTagUuids = qtn ^. selectedTagUuids
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireCreatorUuid = qtn ^. creatorUuid
    , _questionnairePermissions = dto ^. permissions
    , _questionnaireEvents = qtn ^. events
    , _questionnaireVersions = qtn ^. versions
    , _questionnaireCreatedAt = qtn ^. createdAt
    , _questionnaireUpdatedAt = now
    }

fromQuestionnaireCreateDTO ::
     QuestionnaireCreateDTO
  -> U.UUID
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> U.UUID
  -> UTCTime
  -> UTCTime
  -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid visibility sharing currentUserUuid qtnCreatedAt qtnUpdatedAt =
  Questionnaire
    { _questionnaireUuid = qtnUuid
    , _questionnaireName = dto ^. name
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = dto ^. packageId
    , _questionnaireSelectedTagUuids = dto ^. tagUuids
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireCreatorUuid = Just currentUserUuid
    , _questionnairePermissions = [toUserPermRecord currentUserUuid ownerPermissions]
    , _questionnaireEvents = []
    , _questionnaireVersions = []
    , _questionnaireCreatedAt = qtnCreatedAt
    , _questionnaireUpdatedAt = qtnUpdatedAt
    }

fromContentChangeDTO :: Questionnaire -> QuestionnaireContentChangeDTO -> UserDTO -> UTCTime -> Questionnaire
fromContentChangeDTO qtn dto currentUser now =
  let newTodoEvents = fmap (\e -> fromEventChangeDTO e (Just $ currentUser ^. uuid) now) (dto ^. events)
      updatedEvents = qtn ^. events ++ newTodoEvents
   in qtn {_questionnaireEvents = updatedEvents, _questionnaireUpdatedAt = now}
