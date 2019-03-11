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
  , _filledKnowledgeModelTags = km ^. tags
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
toFilledQuestion (OptionsQuestion' q) =
  FilledOptionsQuestion' $
  FilledOptionsQuestion
  { _filledOptionsQuestionUuid = q ^. uuid
  , _filledOptionsQuestionTitle = q ^. title
  , _filledOptionsQuestionText = q ^. text
  , _filledOptionsQuestionRequiredLevel = q ^. requiredLevel
  , _filledOptionsQuestionTagUuids = q ^. tagUuids
  , _filledOptionsQuestionExperts = q ^. experts
  , _filledOptionsQuestionReferences = q ^. references
  , _filledOptionsQuestionAnswers = q ^. answers
  , _filledOptionsQuestionAnswerOption = Nothing
  }
toFilledQuestion (ListQuestion' q) =
  FilledListQuestion' $
  FilledListQuestion
  { _filledListQuestionUuid = q ^. uuid
  , _filledListQuestionTitle = q ^. title
  , _filledListQuestionText = q ^. text
  , _filledListQuestionRequiredLevel = q ^. requiredLevel
  , _filledListQuestionTagUuids = q ^. tagUuids
  , _filledListQuestionExperts = q ^. experts
  , _filledListQuestionReferences = q ^. references
  , _filledListQuestionItemTemplateTitle = q ^. itemTemplateTitle
  , _filledListQuestionItemTemplateQuestions = q ^. itemTemplateQuestions
  , _filledListQuestionItems = Nothing
  }
toFilledQuestion (ValueQuestion' q) =
  FilledValueQuestion' $
  FilledValueQuestion
  { _filledValueQuestionUuid = q ^. uuid
  , _filledValueQuestionTitle = q ^. title
  , _filledValueQuestionText = q ^. text
  , _filledValueQuestionRequiredLevel = q ^. requiredLevel
  , _filledValueQuestionTagUuids = q ^. tagUuids
  , _filledValueQuestionExperts = q ^. experts
  , _filledValueQuestionReferences = q ^. references
  , _filledValueQuestionValueType = q ^. valueType
  , _filledValueQuestionAnswerValue = Nothing
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

toFilledAnswerItem :: FilledListQuestion -> FilledAnswerItem
toFilledAnswerItem fQ =
  FilledAnswerItem
  { _filledAnswerItemTitle = fQ ^. itemTemplateTitle
  , _filledAnswerItemValue = Nothing
  , _filledAnswerItemQuestions = toFilledQuestion <$> fQ ^. itemTemplateQuestions
  }
