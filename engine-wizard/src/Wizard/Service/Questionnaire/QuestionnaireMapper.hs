module Wizard.Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import Data.Time
import Data.UUID (UUID)

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireLabel
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import qualified Wizard.Service.Package.PackageMapper as PM

toDTO :: Questionnaire -> Package -> QuestionnaireState -> Maybe UserDTO -> QuestionnaireDTO
toDTO questionnaire package state mOwner =
  QuestionnaireDTO
    { _questionnaireDTOUuid = questionnaire ^. uuid
    , _questionnaireDTOName = questionnaire ^. name
    , _questionnaireDTOLevel = questionnaire ^. level
    , _questionnaireDTOAccessibility = questionnaire ^. accessibility
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO package
    , _questionnaireDTOOwner = mOwner
    , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
    }

toSimpleDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireState -> Maybe UserDTO -> QuestionnaireDTO
toSimpleDTO questionnaire package state mOwner =
  QuestionnaireDTO
    { _questionnaireDTOUuid = questionnaire ^. uuid
    , _questionnaireDTOName = questionnaire ^. name
    , _questionnaireDTOLevel = questionnaire ^. level
    , _questionnaireDTOAccessibility = questionnaire ^. accessibility
    , _questionnaireDTOState = state
    , _questionnaireDTOPackage = PM.toSimpleDTO . SPM.toPackage $ package
    , _questionnaireDTOOwner = mOwner
    , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
    }

toReplyDTO :: Reply -> ReplyDTO
toReplyDTO reply = ReplyDTO {_replyDTOPath = reply ^. path, _replyDTOValue = toReplyValueDTO $ reply ^. value}

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
    {_integrationValueDTOIntId = _integrationValueIntId, _integrationValueDTOIntValue = _integrationValueIntValue}

toLabelDTO :: Label -> LabelDTO
toLabelDTO label = LabelDTO {_labelDTOPath = label ^. path, _labelDTOValue = label ^. value}

toDetailWithPackageWithEventsDTO ::
     Questionnaire -> PackageWithEvents -> KnowledgeModel -> QuestionnaireState -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO questionnaire package knowledgeModel state =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = questionnaire ^. uuid
    , _questionnaireDetailDTOName = questionnaire ^. name
    , _questionnaireDetailDTOLevel = questionnaire ^. level
    , _questionnaireDetailDTOAccessibility = questionnaire ^. accessibility
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = PM.toSimpleDTO . SPM.toPackage $ package
    , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateUuid = questionnaire ^. templateUuid
    , _questionnaireDetailDTOFormatUuid = questionnaire ^. formatUuid
    , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
    , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
    , _questionnaireDetailDTOLabels = toLabelDTO <$> questionnaire ^. labels
    , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
    , _questionnaireDetailDTOCreatorUuid = questionnaire ^. creatorUuid
    , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
    }

toDetailWithPackageDTO ::
     Questionnaire -> PackageSimpleDTO -> KnowledgeModel -> QuestionnaireState -> QuestionnaireDetailDTO
toDetailWithPackageDTO questionnaire package knowledgeModel state =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid = questionnaire ^. uuid
    , _questionnaireDetailDTOName = questionnaire ^. name
    , _questionnaireDetailDTOLevel = questionnaire ^. level
    , _questionnaireDetailDTOAccessibility = questionnaire ^. accessibility
    , _questionnaireDetailDTOState = state
    , _questionnaireDetailDTOPackage = package
    , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
    , _questionnaireDetailDTOTemplateUuid = questionnaire ^. templateUuid
    , _questionnaireDetailDTOFormatUuid = questionnaire ^. formatUuid
    , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
    , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
    , _questionnaireDetailDTOLabels = toLabelDTO <$> questionnaire ^. labels
    , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
    , _questionnaireDetailDTOCreatorUuid = questionnaire ^. creatorUuid
    , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
    , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
    }

fromReplyDTO :: ReplyDTO -> Reply
fromReplyDTO reply = Reply {_replyPath = reply ^. path, _replyValue = fromReplyValueDTO $ reply ^. value}

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
    {_integrationValueIntId = _integrationValueDTOIntId, _integrationValueIntValue = _integrationValueDTOIntValue}

fromLabelDTO :: LabelDTO -> Label
fromLabelDTO label = Label {_labelPath = label ^. path, _labelValue = label ^. value}

fromChangeDTO ::
     QuestionnaireDetailDTO -> QuestionnaireChangeDTO -> QuestionnaireAccessibility -> UUID -> UTCTime -> Questionnaire
fromChangeDTO qtn dto accessibility currentUserUuid now =
  Questionnaire
    { _questionnaireUuid = qtn ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = dto ^. level
    , _questionnaireAccessibility = accessibility
    , _questionnairePackageId = qtn ^. package . pId
    , _questionnaireSelectedTagUuids = qtn ^. selectedTagUuids
    , _questionnaireTemplateUuid = dto ^. templateUuid
    , _questionnaireFormatUuid = qtn ^. formatUuid
    , _questionnaireReplies = fromReplyDTO <$> dto ^. replies
    , _questionnaireLabels = fromLabelDTO <$> dto ^. labels
    , _questionnaireOwnerUuid =
        if accessibility /= PublicQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _questionnaireCreatorUuid = qtn ^. creatorUuid
    , _questionnaireCreatedAt = qtn ^. createdAt
    , _questionnaireUpdatedAt = now
    }

fromQuestionnaireCreateDTO ::
     QuestionnaireCreateDTO -> UUID -> QuestionnaireAccessibility -> UUID -> UTCTime -> UTCTime -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid accessibility currentUserUuid qtnCreatedAt qtnUpdatedAt =
  Questionnaire
    { _questionnaireUuid = qtnUuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = 1
    , _questionnaireAccessibility = accessibility
    , _questionnairePackageId = dto ^. packageId
    , _questionnaireSelectedTagUuids = dto ^. tagUuids
    , _questionnaireTemplateUuid = dto ^. templateUuid
    , _questionnaireFormatUuid = Nothing
    , _questionnaireReplies = []
    , _questionnaireLabels = []
    , _questionnaireOwnerUuid =
        if accessibility /= PublicQuestionnaire
          then Just currentUserUuid
          else Nothing
    , _questionnaireCreatorUuid = Just currentUserUuid
    , _questionnaireCreatedAt = qtnCreatedAt
    , _questionnaireUpdatedAt = qtnUpdatedAt
    }

fromDetailDTO :: QuestionnaireDetailDTO -> Questionnaire
fromDetailDTO dto =
  Questionnaire
    { _questionnaireUuid = dto ^. uuid
    , _questionnaireName = dto ^. name
    , _questionnaireLevel = dto ^. level
    , _questionnaireAccessibility = dto ^. accessibility
    , _questionnairePackageId = dto ^. package . pId
    , _questionnaireSelectedTagUuids = dto ^. selectedTagUuids
    , _questionnaireTemplateUuid = dto ^. templateUuid
    , _questionnaireFormatUuid = dto ^. formatUuid
    , _questionnaireReplies = fromReplyDTO <$> dto ^. replies
    , _questionnaireLabels = fromLabelDTO <$> dto ^. labels
    , _questionnaireOwnerUuid = dto ^. ownerUuid
    , _questionnaireCreatorUuid = dto ^. creatorUuid
    , _questionnaireCreatedAt = dto ^. createdAt
    , _questionnaireUpdatedAt = dto ^. updatedAt
    }
