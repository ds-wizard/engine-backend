module Service.FilledKnowledgeModel.FilledKnowledgeModelMapper where

import Control.Lens ((^.))

import Api.Resource.FilledKnowledgeModel.FilledKnowledgeModelDTO
import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Service.KnowledgeModel.KnowledgeModelMapper

toFilledKMDTO :: FilledKnowledgeModel -> FilledKnowledgeModelDTO
toFilledKMDTO fKM =
  FilledKnowledgeModelDTO
  { _filledKnowledgeModelDTOUuid = fKM ^. uuid
  , _filledKnowledgeModelDTOName = fKM ^. name
  , _filledKnowledgeModelDTOChapters = toFilledChapterDTO <$> fKM ^. chapters
  , _filledKnowledgeModelDTOTags = toTagDTO <$> fKM ^. tags
  }

toFilledChapterDTO :: FilledChapter -> FilledChapterDTO
toFilledChapterDTO fCh =
  FilledChapterDTO
  { _filledChapterDTOUuid = fCh ^. uuid
  , _filledChapterDTOTitle = fCh ^. title
  , _filledChapterDTOText = fCh ^. text
  , _filledChapterDTOQuestions = toFilledQuestionDTO <$> fCh ^. questions
  }

toFilledQuestionDTO :: FilledQuestion -> FilledQuestionDTO
toFilledQuestionDTO (FilledOptionsQuestion' fQ) =
  FilledOptionsQuestionDTO'
    FilledOptionsQuestionDTO
    { _filledOptionsQuestionDTOUuid = fQ ^. uuid
    , _filledOptionsQuestionDTOTitle = fQ ^. title
    , _filledOptionsQuestionDTOText = fQ ^. text
    , _filledOptionsQuestionDTORequiredLevel = fQ ^. requiredLevel
    , _filledOptionsQuestionDTOTagUuids = fQ ^. tagUuids
    , _filledOptionsQuestionDTOExperts = toExpertDTO <$> fQ ^. experts
    , _filledOptionsQuestionDTOReferences = toReferenceDTO <$> fQ ^. references
    , _filledOptionsQuestionDTOAnswers = toAnswerDTO <$> fQ ^. answers
    , _filledOptionsQuestionDTOAnswerOption = toFilledAnswerDTO <$> fQ ^. answerOption
    }
toFilledQuestionDTO (FilledListQuestion' fQ) =
  FilledListQuestionDTO'
    FilledListQuestionDTO
    { _filledListQuestionDTOUuid = fQ ^. uuid
    , _filledListQuestionDTOTitle = fQ ^. title
    , _filledListQuestionDTOText = fQ ^. text
    , _filledListQuestionDTORequiredLevel = fQ ^. requiredLevel
    , _filledListQuestionDTOTagUuids = fQ ^. tagUuids
    , _filledListQuestionDTOExperts = toExpertDTO <$> fQ ^. experts
    , _filledListQuestionDTOReferences = toReferenceDTO <$> fQ ^. references
    , _filledListQuestionDTOItemTemplateTitle = fQ ^. itemTemplateTitle
    , _filledListQuestionDTOItemTemplateQuestions = toQuestionDTO <$> fQ ^. itemTemplateQuestions
    , _filledListQuestionDTOItems = (fmap toFilledAnswerItemDTO) <$> fQ ^. items
    }
toFilledQuestionDTO (FilledValueQuestion' fQ) =
  FilledValueQuestionDTO'
    FilledValueQuestionDTO
    { _filledValueQuestionDTOUuid = fQ ^. uuid
    , _filledValueQuestionDTOTitle = fQ ^. title
    , _filledValueQuestionDTOText = fQ ^. text
    , _filledValueQuestionDTORequiredLevel = fQ ^. requiredLevel
    , _filledValueQuestionDTOTagUuids = fQ ^. tagUuids
    , _filledValueQuestionDTOExperts = toExpertDTO <$> fQ ^. experts
    , _filledValueQuestionDTOReferences = toReferenceDTO <$> fQ ^. references
    , _filledValueQuestionDTOValueType = fQ ^. valueType
    , _filledValueQuestionDTOAnswerValue = fQ ^. answerValue
    }

toFilledAnswerDTO :: FilledAnswer -> FilledAnswerDTO
toFilledAnswerDTO fAns =
  FilledAnswerDTO
  { _filledAnswerDTOUuid = fAns ^. uuid
  , _filledAnswerDTOLabel = fAns ^. label
  , _filledAnswerDTOAdvice = fAns ^. advice
  , _filledAnswerDTOFollowUps = toFilledQuestionDTO <$> fAns ^. followUps
  , _filledAnswerDTOMetricMeasures = toMetricMeasureDTO <$> fAns ^. metricMeasures
  }

toFilledAnswerItemDTO :: FilledAnswerItem -> FilledAnswerItemDTO
toFilledAnswerItemDTO fAi =
  FilledAnswerItemDTO
  { _filledAnswerItemDTOTitle = fAi ^. title
  , _filledAnswerItemDTOValue = fAi ^. value
  , _filledAnswerItemDTOQuestions = toFilledQuestionDTO <$> fAi ^. questions
  }
