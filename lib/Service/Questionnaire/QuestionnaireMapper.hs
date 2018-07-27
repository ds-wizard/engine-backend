module Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import Data.Time
import Data.UUID (UUID)

import Api.Resource.Package.PackageDTO
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.Package.Package
import Model.Questionnaire.Questionnaire
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.Package.PackageMapper

toDTO :: Questionnaire -> Package -> QuestionnaireDTO
toDTO questionnaire package =
  QuestionnaireDTO
  { _questionnaireDTOUuid = questionnaire ^. uuid
  , _questionnaireDTOName = questionnaire ^. name
  , _questionnaireDTOLevel = questionnaire ^. level
  , _questionnaireDTOPackage = packageToDTO package
  , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
  }

toSimpleDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireDTO
toSimpleDTO questionnaire package =
  QuestionnaireDTO
  { _questionnaireDTOUuid = questionnaire ^. uuid
  , _questionnaireDTOName = questionnaire ^. name
  , _questionnaireDTOLevel = questionnaire ^. level
  , _questionnaireDTOPackage = packageWithEventsToDTO package
  , _questionnaireDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDTOUpdatedAt = questionnaire ^. updatedAt
  }

toReplyDTO :: QuestionnaireReply -> QuestionnaireReplyDTO
toReplyDTO reply =
  QuestionnaireReplyDTO {_questionnaireReplyDTOPath = reply ^. path, _questionnaireReplyDTOValue = reply ^. value}

toDetailWithPackageWithEventsDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireDetailDTO
toDetailWithPackageWithEventsDTO questionnaire package =
  QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid = questionnaire ^. uuid
  , _questionnaireDetailDTOName = questionnaire ^. name
  , _questionnaireDetailDTOLevel = questionnaire ^. level
  , _questionnaireDetailDTOPackage = packageWithEventsToDTO package
  , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO $ questionnaire ^. knowledgeModel
  , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
  , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
  }

toDetailWithPackageDTODTO :: Questionnaire -> PackageDTO -> QuestionnaireDetailDTO
toDetailWithPackageDTODTO questionnaire package =
  QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid = questionnaire ^. uuid
  , _questionnaireDetailDTOName = questionnaire ^. name
  , _questionnaireDetailDTOLevel = questionnaire ^. level
  , _questionnaireDetailDTOPackage = package
  , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO $ questionnaire ^. knowledgeModel
  , _questionnaireDetailDTOReplies = toReplyDTO <$> questionnaire ^. replies
  , _questionnaireDetailDTOCreatedAt = questionnaire ^. createdAt
  , _questionnaireDetailDTOUpdatedAt = questionnaire ^. updatedAt
  }

fromReplyDTO :: QuestionnaireReplyDTO -> QuestionnaireReply
fromReplyDTO reply =
  QuestionnaireReply {_questionnaireReplyPath = reply ^. path, _questionnaireReplyValue = reply ^. value}

fromChangeDTO :: QuestionnaireDetailDTO -> QuestionnaireChangeDTO -> UTCTime -> Questionnaire
fromChangeDTO qtn dto now =
  Questionnaire
  { _questionnaireUuid = qtn ^. uuid
  , _questionnaireName = qtn ^. name
  , _questionnaireLevel = dto ^. level
  , _questionnairePackageId = qtn ^. package . pId
  , _questionnaireKnowledgeModel = fromKnowledgeModelDTO $ qtn ^. knowledgeModel
  , _questionnaireReplies = fromReplyDTO <$> dto ^. replies
  , _questionnaireCreatedAt = qtn ^. createdAt
  , _questionnaireUpdatedAt = now
  }

fromQuestionnaireCreateDTO :: QuestionnaireCreateDTO -> UUID -> KnowledgeModel -> UTCTime -> UTCTime -> Questionnaire
fromQuestionnaireCreateDTO dto qtnUuid knowledgeModel qtnCreatedAt qtnUpdatedAt =
  Questionnaire
  { _questionnaireUuid = qtnUuid
  , _questionnaireName = dto ^. name
  , _questionnaireLevel = dto ^. level
  , _questionnairePackageId = dto ^. packageId
  , _questionnaireKnowledgeModel = knowledgeModel
  , _questionnaireReplies = []
  , _questionnaireCreatedAt = qtnCreatedAt
  , _questionnaireUpdatedAt = qtnUpdatedAt
  }
