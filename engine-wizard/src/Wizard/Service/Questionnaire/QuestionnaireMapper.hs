module Wizard.Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Data.Time
import Data.UUID (UUID)

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Report.Report
import qualified Wizard.Service.Package.PackageMapper as PM

toDTO :: Questionnaire -> Package -> QuestionnaireState -> Maybe UserDTO -> QuestionnaireReportDTO -> QuestionnaireDTO
toDTO questionnaire package state mOwner report =
  QuestionnaireDTO
    { _questionnaireDTOUuid = questionnaire ^. uuid
    , _questionnaireDTOName = questionnaire ^. name
    , _questionnaireDTOLevel = questionnaire ^. level
    , _questionnaireDTOVisibility = questionnaire ^. visibility
    , _questionnaireDTOSharing = questionnaire ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO package
    , _questionnaireDTOOwner = mOwner
    , _questionnaireDTOReport = report
    , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
    }

toSimpleDTO ::
     Questionnaire
  -> PackageWithEvents
  -> QuestionnaireState
  -> Maybe UserDTO
  -> QuestionnaireReportDTO
  -> QuestionnaireDTO
toSimpleDTO questionnaire package state mOwner report =
  QuestionnaireDTO
    { _questionnaireDTOUuid = questionnaire ^. uuid
    , _questionnaireDTOName = questionnaire ^. name
    , _questionnaireDTOLevel = questionnaire ^. level
    , _questionnaireDTOVisibility = questionnaire ^. visibility
    , _questionnaireDTOSharing = questionnaire ^. sharing
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO . SPM.toPackage $ package
    , _questionnaireDTOOwner = mOwner
    , _questionnaireDTOReport = report
    , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
    }

toReplyValueDTO :: ReplyValue -> ReplyValueDTO
toReplyValueDTO StringReply {..} = StringReplyDTO {_stringReplyDTOValue = _stringReplyValue}
toReplyValueDTO AnswerReply {..} = AnswerReplyDTO {_answerReplyDTOValue = _answerReplyValue}
toReplyValueDTO ItemListReply {..} = ItemListReplyDTO {_itemListReplyDTOValue = _itemListReplyValue}
toReplyValueDTO IntegrationReply {..} =
  IntegrationReplyDTO {_integrationReplyDTOValue = toIntegrationReplyValueDTO _integrationReplyValue}

toIntegrationReplyValueDTO :: IntegrationReplyValue -> IntegrationReplyValueDTO
toIntegrationReplyValueDTO (PlainValue reply) = PlainValueDTO reply
toIntegrationReplyValueDTO IntegrationValue {..} =
  IntegrationValueDTO
    {_integrationValueDTOIntId = _integrationValueIntId, _integrationValueDTOValue = _integrationValueValue}

toDetailWithPackageWithEventsDTO ::
     Questionnaire
  -> PackageWithEvents
  -> KnowledgeModel
  -> QuestionnaireState
  -> QuestionnaireReportDTO
  -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO questionnaire package knowledgeModel state report =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = questionnaire ^. uuid
    , _questionnaireDetailDTOName = questionnaire ^. name
    , _questionnaireDetailDTOLevel = questionnaire ^. level
    , _questionnaireDetailDTOVisibility = questionnaire ^. visibility
    , _questionnaireDetailDTOSharing = questionnaire ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = PM.toSimpleDTO . SPM.toPackage $ package
    , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateId = questionnaire ^. templateId
    , _questionnaireDetailDTOFormatUuid = questionnaire ^. formatUuid
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = M.map toReplyValueDTO (questionnaire ^. replies)
    , _questionnaireDetailDTOLabels = questionnaire ^. labels
    , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
    , _questionnaireDetailDTOReport = report
    , _questionnaireDetailDTOCreatorUuid = questionnaire ^. creatorUuid
    , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
    }

toDetailWithPackageDTO ::
     Questionnaire
  -> PackageSimpleDTO
  -> KnowledgeModel
  -> QuestionnaireState
  -> QuestionnaireReportDTO
  -> QuestionnaireDetailDTO
toDetailWithPackageDTO questionnaire package knowledgeModel state report =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = questionnaire ^. uuid
    , _questionnaireDetailDTOName = questionnaire ^. name
    , _questionnaireDetailDTOLevel = questionnaire ^. level
    , _questionnaireDetailDTOVisibility = questionnaire ^. visibility
    , _questionnaireDetailDTOSharing = questionnaire ^. sharing
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = package
    , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateId = questionnaire ^. templateId
    , _questionnaireDetailDTOFormatUuid = questionnaire ^. formatUuid
    , _questionnaireDetailDTOKnowledgeModel = knowledgeModel
    , _questionnaireDetailDTOReplies = M.map toReplyValueDTO (questionnaire ^. replies)
    , _questionnaireDetailDTOLabels = questionnaire ^. labels
    , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
    , _questionnaireDetailDTOReport = report
    , _questionnaireDetailDTOCreatorUuid = questionnaire ^. creatorUuid
    , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
    }

toQuestionnaireReportDTO :: [Indication] -> QuestionnaireReportDTO
toQuestionnaireReportDTO indications = QuestionnaireReportDTO {_questionnaireReportDTOIndications = indications}

toChangeDTO :: Questionnaire -> QuestionnaireChangeDTO
toChangeDTO dto =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = dto ^. name
    , _questionnaireChangeDTOVisibility = dto ^. visibility
    , _questionnaireChangeDTOSharing = dto ^. sharing
    , _questionnaireChangeDTOTemplateId = dto ^. templateId
    }

fromReplyValueDTO :: ReplyValueDTO -> ReplyValue
fromReplyValueDTO StringReplyDTO {..} = StringReply {_stringReplyValue = _stringReplyDTOValue}
fromReplyValueDTO AnswerReplyDTO {..} = AnswerReply {_answerReplyValue = _answerReplyDTOValue}
fromReplyValueDTO ItemListReplyDTO {..} = ItemListReply {_itemListReplyValue = _itemListReplyDTOValue}
fromReplyValueDTO IntegrationReplyDTO {..} =
  IntegrationReply {_integrationReplyValue = fromIntegrationReplyValueDTO _integrationReplyDTOValue}

fromIntegrationReplyValueDTO :: IntegrationReplyValueDTO -> IntegrationReplyValue
fromIntegrationReplyValueDTO (PlainValueDTO reply) = PlainValue reply
fromIntegrationReplyValueDTO IntegrationValueDTO {..} =
  IntegrationValue
    {_integrationValueIntId = _integrationValueDTOIntId, _integrationValueValue = _integrationValueDTOValue}

fromChangeDTO ::
     QuestionnaireDetailDTO
  -> QuestionnaireChangeDTO
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> UUID
  -> UTCTime
  -> Questionnaire
fromChangeDTO qtn dto visibility sharing currentUserUuid now =
  Questionnaire
    { _questionnaireUuid = qtn ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = qtn ^. level
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = qtn ^. package . pId
    , _questionnaireSelectedTagUuids = qtn ^. selectedTagUuids
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = qtn ^. formatUuid
    , _questionnaireReplies = M.map fromReplyValueDTO (qtn ^. replies)
    , _questionnaireLabels = qtn ^. labels
    , _questionnaireOwnerUuid =
        if visibility /= VisibleEditQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _questionnaireCreatorUuid = qtn ^. creatorUuid
    , _questionnaireCreatedAt = qtn ^. createdAt
    , _questionnaireUpdatedAt = now
    }

fromQuestionnaireCreateDTO ::
     QuestionnaireCreateDTO
  -> UUID
  -> QuestionnaireVisibility
  -> QuestionnaireSharing
  -> UUID
  -> UTCTime
  -> UTCTime
  -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid visibility sharing currentUserUuid qtnCreatedAt qtnUpdatedAt =
  Questionnaire
    { _questionnaireUuid = qtnUuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = 1
    , _questionnaireVisibility = visibility
    , _questionnaireSharing = sharing
    , _questionnairePackageId = dto ^. packageId
    , _questionnaireSelectedTagUuids = dto ^. tagUuids
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = Nothing
    , _questionnaireReplies = M.empty
    , _questionnaireLabels = M.empty
    , _questionnaireOwnerUuid =
        if visibility /= VisibleEditQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _questionnaireCreatorUuid = Just currentUserUuid
    , _questionnaireCreatedAt = qtnCreatedAt
    , _questionnaireUpdatedAt = qtnUpdatedAt
    }

fromContentChangeDTO :: Questionnaire -> QuestionnaireContentChangeDTO -> UTCTime -> Questionnaire
fromContentChangeDTO qtn dto now =
  qtn
    { _questionnaireLevel = dto ^. level
    , _questionnaireReplies = M.map fromReplyValueDTO (dto ^. replies)
    , _questionnaireLabels = dto ^. labels
    , _questionnaireUpdatedAt = now
    }

fromDetailDTO :: QuestionnaireDetailDTO -> Questionnaire
fromDetailDTO dto =
  Questionnaire
    { _questionnaireUuid = dto ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = dto ^. level
    , _questionnaireVisibility = dto ^. visibility
    , _questionnaireSharing = dto ^. sharing
    , _questionnairePackageId = dto ^. package . pId
    , _questionnaireSelectedTagUuids = dto ^. selectedTagUuids
    , _questionnaireTemplateId = dto ^. templateId
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireReplies = M.map fromReplyValueDTO (dto ^. replies)
    , _questionnaireLabels = dto ^. labels
    , _questionnaireOwnerUuid = dto ^. ownerUuid
    , _questionnaireCreatorUuid = dto ^. creatorUuid
    , _questionnaireCreatedAt = dto ^. createdAt
    , _questionnaireUpdatedAt = dto ^. updatedAt
    }
