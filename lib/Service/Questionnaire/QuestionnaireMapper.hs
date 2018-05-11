module Service.Questionnaire.QuestionnaireMapper where

import Control.Lens ((^.))
import Data.Map
import Data.UUID (UUID)

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
  , _questionnaireDTOPackage = packageToDTO package
  }

toSimpleDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireDTO
toSimpleDTO questionnaire package =
  QuestionnaireDTO
  { _questionnaireDTOUuid = questionnaire ^. uuid
  , _questionnaireDTOName = questionnaire ^. name
  , _questionnaireDTOPackage = packageWithEventsToDTO package
  }

toDetailDTO :: Questionnaire -> PackageWithEvents -> QuestionnaireDetailDTO
toDetailDTO questionnaire package =
  QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid = questionnaire ^. uuid
  , _questionnaireDetailDTOName = questionnaire ^. name
  , _questionnaireDetailDTOPackage = packageWithEventsToDTO package
  , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO $ questionnaire ^. knowledgeModel
  , _questionnaireDetailDTOReplies = questionnaire ^. replies
  }

fromQuestionnaireCreateDTO :: QuestionnaireCreateDTO -> UUID -> KnowledgeModel -> Questionnaire
fromQuestionnaireCreateDTO dto questionnaireUuid knowledgeModel =
  Questionnaire
  { _questionnaireUuid = questionnaireUuid
  , _questionnaireName = dto ^. name
  , _questionnairePackageId = dto ^. packageId
  , _questionnaireKnowledgeModel = knowledgeModel
  , _questionnaireReplies = empty
  }
