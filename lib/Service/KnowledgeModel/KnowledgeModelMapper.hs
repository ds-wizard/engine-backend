module Service.KnowledgeModel.KnowledgeModelMapper where

import Control.Lens ((^.))

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

toKnowledgeModelDTO :: KnowledgeModel -> KnowledgeModelDTO
toKnowledgeModelDTO km =
  KnowledgeModelDTO
  { _knowledgeModelDTOUuid = km ^. uuid
  , _knowledgeModelDTOName = km ^. name
  , _knowledgeModelDTOTags = toTagDTO <$> (km ^. tags)
  , _knowledgeModelDTOChapters = toChapterDTO <$> (km ^. chapters)
  }

toChapterDTO :: Chapter -> ChapterDTO
toChapterDTO chapter =
  ChapterDTO
  { _chapterDTOUuid = chapter ^. uuid
  , _chapterDTOTitle = chapter ^. title
  , _chapterDTOText = chapter ^. text
  , _chapterDTOQuestions = toQuestionDTO <$> (chapter ^. questions)
  }

toQuestionDTO :: Question -> QuestionDTO
toQuestionDTO question =
  QuestionDTO
  { _questionDTOUuid = question ^. uuid
  , _questionDTOQType = question ^. qType
  , _questionDTOTitle = question ^. title
  , _questionDTOText = question ^. text
  , _questionDTORequiredLevel = question ^. requiredLevel
  , _questionDTOTagUuids = question ^. tagUuids
  , _questionDTOAnswers = (fmap toAnswerDTO) <$> (question ^. answers)
  , _questionDTOAnswerItemTemplate = toAnswerItemTemplateDTO <$> (question ^. answerItemTemplate)
  , _questionDTOExperts = toExpertDTO <$> (question ^. experts)
  , _questionDTOReferences = toReferenceDTO <$> (question ^. references)
  }

toAnswerDTO :: Answer -> AnswerDTO
toAnswerDTO answer =
  AnswerDTO
  { _answerDTOUuid = answer ^. uuid
  , _answerDTOLabel = answer ^. label
  , _answerDTOAdvice = answer ^. advice
  , _answerDTOFollowUps = toQuestionDTO <$> (answer ^. followUps)
  , _answerDTOMetricMeasures = toMetricMeasureDTO <$> (answer ^. metricMeasures)
  }

toAnswerItemTemplateDTO :: AnswerItemTemplate -> AnswerItemTemplateDTO
toAnswerItemTemplateDTO itemTemplate =
  AnswerItemTemplateDTO
  { _answerItemTemplateDTOTitle = itemTemplate ^. title
  , _answerItemTemplateDTOQuestions = toQuestionDTO <$> itemTemplate ^. questions
  }

toAnswerItemTemplatePlainDTO :: AnswerItemTemplatePlain -> AnswerItemTemplatePlainDTO
toAnswerItemTemplatePlainDTO itemTemplate =
  AnswerItemTemplatePlainDTO {_answerItemTemplatePlainDTOTitle = itemTemplate ^. title}

toAnswerItemTemplatePlainWithUuidsDTO :: AnswerItemTemplatePlainWithUuids -> AnswerItemTemplatePlainWithUuidsDTO
toAnswerItemTemplatePlainWithUuidsDTO itemTemplate =
  AnswerItemTemplatePlainWithUuidsDTO
  { _answerItemTemplatePlainWithUuidsDTOTitle = itemTemplate ^. title
  , _answerItemTemplatePlainWithUuidsDTOQuestionUuids = itemTemplate ^. questionUuids
  }

toExpertDTO :: Expert -> ExpertDTO
toExpertDTO expert =
  ExpertDTO {_expertDTOUuid = expert ^. uuid, _expertDTOName = expert ^. name, _expertDTOEmail = expert ^. email}

toReferenceDTO :: Reference -> ReferenceDTO
toReferenceDTO (ResourcePageReference' reference) =
  ResourcePageReferenceDTO'
    ResourcePageReferenceDTO
    {_resourcePageReferenceDTOUuid = reference ^. uuid, _resourcePageReferenceDTOShortUuid = reference ^. shortUuid}
toReferenceDTO (URLReference' reference) =
  URLReferenceDTO'
    URLReferenceDTO
    { _uRLReferenceDTOUuid = reference ^. uuid
    , _uRLReferenceDTOUrl = reference ^. url
    , _uRLReferenceDTOLabel = reference ^. label
    }
toReferenceDTO (CrossReference' reference) =
  CrossReferenceDTO'
    CrossReferenceDTO
    { _crossReferenceDTOUuid = reference ^. uuid
    , _crossReferenceDTOTargetUuid = reference ^. targetUuid
    , _crossReferenceDTODescription = reference ^. description
    }

toMetricMeasureDTO :: MetricMeasure -> MetricMeasureDTO
toMetricMeasureDTO m =
  MetricMeasureDTO
  { _metricMeasureDTOMetricUuid = m ^. metricUuid
  , _metricMeasureDTOMeasure = m ^. measure
  , _metricMeasureDTOWeight = m ^. weight
  }

toTagDTO :: Tag -> TagDTO
toTagDTO tag =
  TagDTO
  { _tagDTOUuid = tag ^. uuid
  , _tagDTOName = tag ^. name
  , _tagDTODescription = tag ^. description
  , _tagDTOColor = tag ^. color
  }

-- ----------------------------------------
-- ----------------------------------------
fromKnowledgeModelDTO :: KnowledgeModelDTO -> KnowledgeModel
fromKnowledgeModelDTO km =
  KnowledgeModel
  { _knowledgeModelUuid = km ^. uuid
  , _knowledgeModelName = km ^. name
  , _knowledgeModelTags = fromTagDTO <$> (km ^. tags)
  , _knowledgeModelChapters = fromChapterDTO <$> (km ^. chapters)
  }

fromChapterDTO :: ChapterDTO -> Chapter
fromChapterDTO chapter =
  Chapter
  { _chapterUuid = chapter ^. uuid
  , _chapterTitle = chapter ^. title
  , _chapterText = chapter ^. text
  , _chapterQuestions = fromQuestionDTO <$> (chapter ^. questions)
  }

fromQuestionDTO :: QuestionDTO -> Question
fromQuestionDTO question =
  Question
  { _questionUuid = question ^. uuid
  , _questionQType = question ^. qType
  , _questionTitle = question ^. title
  , _questionText = question ^. text
  , _questionRequiredLevel = question ^. requiredLevel
  , _questionTagUuids = question ^. tagUuids
  , _questionAnswers = (fmap fromAnswerDTO) <$> (question ^. answers)
  , _questionAnswerItemTemplate = fromAnswerItemTemplateDTO <$> (question ^. answerItemTemplate)
  , _questionExperts = fromExpertDTO <$> (question ^. experts)
  , _questionReferences = fromReferenceDTO <$> (question ^. references)
  }

fromAnswerDTO :: AnswerDTO -> Answer
fromAnswerDTO answer =
  Answer
  { _answerUuid = answer ^. uuid
  , _answerLabel = answer ^. label
  , _answerAdvice = answer ^. advice
  , _answerFollowUps = fromQuestionDTO <$> (answer ^. followUps)
  , _answerMetricMeasures = fromMetricMeasureDTO <$> (answer ^. metricMeasures)
  }

fromAnswerItemTemplateDTO :: AnswerItemTemplateDTO -> AnswerItemTemplate
fromAnswerItemTemplateDTO itemTemplate =
  AnswerItemTemplate
  { _answerItemTemplateTitle = itemTemplate ^. title
  , _answerItemTemplateQuestions = fromQuestionDTO <$> itemTemplate ^. questions
  }

fromAnswerItemTemplatePlainDTO :: AnswerItemTemplatePlainDTO -> AnswerItemTemplatePlain
fromAnswerItemTemplatePlainDTO itemTemplate =
  AnswerItemTemplatePlain {_answerItemTemplatePlainTitle = itemTemplate ^. title}

fromAnswerItemTemplatePlainWithUuidsDTO :: AnswerItemTemplatePlainWithUuidsDTO -> AnswerItemTemplatePlainWithUuids
fromAnswerItemTemplatePlainWithUuidsDTO itemTemplate =
  AnswerItemTemplatePlainWithUuids
  { _answerItemTemplatePlainWithUuidsTitle = itemTemplate ^. title
  , _answerItemTemplatePlainWithUuidsQuestionUuids = itemTemplate ^. questionUuids
  }

fromExpertDTO :: ExpertDTO -> Expert
fromExpertDTO expert =
  Expert {_expertUuid = expert ^. uuid, _expertName = expert ^. name, _expertEmail = expert ^. email}

fromReferenceDTO :: ReferenceDTO -> Reference
fromReferenceDTO (ResourcePageReferenceDTO' reference) =
  ResourcePageReference'
    ResourcePageReference
    {_resourcePageReferenceUuid = reference ^. uuid, _resourcePageReferenceShortUuid = reference ^. shortUuid}
fromReferenceDTO (URLReferenceDTO' reference) =
  URLReference'
    URLReference
    { _uRLReferenceUuid = reference ^. uuid
    , _uRLReferenceUrl = reference ^. url
    , _uRLReferenceLabel = reference ^. label
    }
fromReferenceDTO (CrossReferenceDTO' reference) =
  CrossReference'
    CrossReference
    { _crossReferenceUuid = reference ^. uuid
    , _crossReferenceTargetUuid = reference ^. targetUuid
    , _crossReferenceDescription = reference ^. description
    }

fromMetricMeasureDTO :: MetricMeasureDTO -> MetricMeasure
fromMetricMeasureDTO m =
  MetricMeasure
  {_metricMeasureMetricUuid = m ^. metricUuid, _metricMeasureMeasure = m ^. measure, _metricMeasureWeight = m ^. weight}

fromTagDTO :: TagDTO -> Tag
fromTagDTO tag =
  Tag {_tagUuid = tag ^. uuid, _tagName = tag ^. name, _tagDescription = tag ^. description, _tagColor = tag ^. color}
