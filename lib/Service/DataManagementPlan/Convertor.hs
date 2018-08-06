module Service.DataManagementPlan.Convertor where

import Control.Lens ((^.))

import LensesConfig
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.KnowledgeModel.KnowledgeModel

toFilledKM :: KnowledgeModel -> FilledKnowledgeModel
toFilledKM km =
  FilledKnowledgeModel
  { _filledKnowledgeModelUuid = km ^. uuid
  , _filledKnowledgeModelName = km ^. name
  , _filledKnowledgeModelChapters = toFilledChapter <$> km ^. chapters
  }

toFilledChapter :: Chapter -> FilledChapter
toFilledChapter ch =
  FilledChapter
  { _filledChapterUuid = ch ^. uuid
  , _filledChapterTitle = ch ^. title
  , _filledChapterText = ch ^. text
  , _filledChapterQuestions = toFilledQuestion <$> ch ^. questions
  }

toFilledQuestion :: Question -> FilledQuestion
toFilledQuestion q =
  FilledQuestion
  { _filledQuestionUuid = q ^. uuid
  , _filledQuestionQType = q ^. qType
  , _filledQuestionTitle = q ^. title
  , _filledQuestionText = q ^. text
  , _filledQuestionAnswerItemTemplate = q ^. answerItemTemplate
  , _filledQuestionAnswers = q ^. answers
  , _filledQuestionAnswerValue = Nothing
  , _filledQuestionAnswerOption = Nothing
  , _filledQuestionAnswerItems = Nothing
  , _filledQuestionExperts = q ^. experts
  , _filledQuestionReferences = q ^. references
  }

toFilledAnswer :: Answer -> FilledAnswer
toFilledAnswer ans =
  FilledAnswer
  { _filledAnswerUuid = ans ^. uuid
  , _filledAnswerLabel = ans ^. label
  , _filledAnswerAdvice = ans ^. advice
  , _filledAnswerFollowUps = toFilledQuestion <$> ans ^. followUps
  , _filledAnswerMetricMeasures = ans ^. metricMeasures
  }

toFilledAnswerItem :: AnswerItemTemplate -> String -> FilledAnswerItem
toFilledAnswerItem ait titleValue =
  FilledAnswerItem
  { _filledAnswerItemTitle = ait ^. title
  , _filledAnswerItemValue = titleValue
  , _filledAnswerItemQuestions = toFilledQuestion <$> ait ^. questions
  }
