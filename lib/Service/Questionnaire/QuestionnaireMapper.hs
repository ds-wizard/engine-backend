module Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import Data.Time
import Data.UUID (UUID)

import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
import Model.Package.PackageWithEvents
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply
import Service.KnowledgeModel.KnowledgeModelMapper
import qualified Service.Package.PackageMapper as PM

toDTO :: Questionnaire -> Package -> QuestionnaireDTO
toDTO questionnaire package =
  QuestionnaireDTO
  { _questionnaireDTOUuid = questionnaire ^. uuid
  , _questionnaireDTOName = questionnaire ^. name
  , _questionnaireDTOLevel = questionnaire ^. level
  , _questionnaireDTOAccessibility = questionnaire ^. accessibility
  , _questionnaireDTOPackage = PM.toSimpleDTO package
  , _questionnaireDTOOwnerUuid = questionnaire ^. ownerUuid
  , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
  }

toSimpleDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireDTO
toSimpleDTO questionnaire package =
  QuestionnaireDTO
  { _questionnaireDTOUuid = questionnaire ^. uuid
  , _questionnaireDTOName = questionnaire ^. name
  , _questionnaireDTOLevel = questionnaire ^. level
  , _questionnaireDTOAccessibility = questionnaire ^. accessibility
  , _questionnaireDTOPackage = PM.toSimpleDTO . PM.toPackage $ package
  , _questionnaireDTOOwnerUuid = questionnaire ^. ownerUuid
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

toDetailWithPackageWithEventsDTO :: Questionnaire -> PackageWithEvents -> KnowledgeModel -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO questionnaire package knowledgeModel =
  QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid = questionnaire ^. uuid
  , _questionnaireDetailDTOName = questionnaire ^. name
  , _questionnaireDetailDTOLevel = questionnaire ^. level
  , _questionnaireDetailDTOAccessibility = questionnaire ^. accessibility
  , _questionnaireDetailDTOPackage = PM.toSimpleDTO . PM.toPackage $ package
  , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
  , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
  , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
  , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
  , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
  }

toDetailWithPackageDTO :: Questionnaire -> PackageSimpleDTO -> KnowledgeModel -> QuestionnaireDetailDTO
toDetailWithPackageDTO questionnaire package knowledgeModel =
  QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid = questionnaire ^. uuid
  , _questionnaireDetailDTOName = questionnaire ^. name
  , _questionnaireDetailDTOLevel = questionnaire ^. level
  , _questionnaireDetailDTOAccessibility = questionnaire ^. accessibility
  , _questionnaireDetailDTOPackage = package
  , _questionnaireDetailDTOSelectedTagUuids = questionnaire ^. selectedTagUuids
  , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO knowledgeModel
  , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
  , _questionnaireDetailDTOOwnerUuid = questionnaire ^. ownerUuid
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
  , _questionnaireReplies = fromReplyDTO <$> dto ^. replies
  , _questionnaireOwnerUuid =
      if accessibility /= PublicQuestionnaire
        then Just currentUserUuid
        else Nothing
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
  , _questionnaireReplies = []
  , _questionnaireOwnerUuid =
      if accessibility /= PublicQuestionnaire
        then Just currentUserUuid
        else Nothing
  , _questionnaireCreatedAt = qtnCreatedAt
  , _questionnaireUpdatedAt = qtnUpdatedAt
  }
