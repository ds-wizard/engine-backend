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
toFilledQuestionDTO fQ =
  FilledQuestionDTO
  { _filledQuestionDTOUuid = fQ ^. uuid
  , _filledQuestionDTOQType = fQ ^. qType
  , _filledQuestionDTOTitle = fQ ^. title
  , _filledQuestionDTOText = fQ ^. text
  , _filledQuestionDTOAnswerItemTemplate = toAnswerItemTemplateDTO <$> fQ ^. answerItemTemplate
  , _filledQuestionDTOAnswers = (fmap toAnswerDTO) <$> fQ ^. answers
  , _filledQuestionDTOAnswerValue = fQ ^. answerValue
  , _filledQuestionDTOAnswerOption = toFilledAnswerDTO <$> fQ ^. answerOption
  , _filledQuestionDTOAnswerItems = (fmap toFilledAnswerItemDTO) <$> fQ ^. answerItems
  , _filledQuestionDTOExperts = toExpertDTO <$> fQ ^. experts
  , _filledQuestionDTOReferences = toReferenceDTO <$> fQ ^. references
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
