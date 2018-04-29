module Service.KnowledgeModel.KnowledgeModelMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

toKnowledgeModelDTO :: KnowledgeModel -> KnowledgeModelDTO
toKnowledgeModelDTO km =
  KnowledgeModelDTO
  { _knowledgeModelDTOUuid = km ^. uuid
  , _knowledgeModelDTOName = km ^. name
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
  , _questionDTOShortUuid = question ^. shortUuid
  , _questionDTOQType = question ^. qType
  , _questionDTOTitle = question ^. title
  , _questionDTOText = question ^. text
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

toAnswerItemTemplatePlainWithIdsDTO :: AnswerItemTemplatePlainWithIds -> AnswerItemTemplatePlainWithIdsDTO
toAnswerItemTemplatePlainWithIdsDTO itemTemplate =
  AnswerItemTemplatePlainWithIdsDTO
  { _answerItemTemplatePlainWithIdsDTOTitle = itemTemplate ^. title
  , _answerItemTemplatePlainWithIdsDTOQuestionIds = itemTemplate ^. questionIds
  }

toExpertDTO :: Expert -> ExpertDTO
toExpertDTO expert =
  ExpertDTO {_expertDTOUuid = expert ^. uuid, _expertDTOName = expert ^. name, _expertDTOEmail = expert ^. email}

toReferenceDTO :: Reference -> ReferenceDTO
toReferenceDTO reference =
  ReferenceDTO {_referenceDTOUuid = reference ^. uuid, _referenceDTOChapter = reference ^. chapter}

fromKnowledgeModelDTO :: KnowledgeModelDTO -> KnowledgeModel
fromKnowledgeModelDTO km =
  KnowledgeModel
  { _knowledgeModelUuid = km ^. uuid
  , _knowledgeModelName = km ^. name
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
  , _questionShortUuid = question ^. shortUuid
  , _questionQType = question ^. qType
  , _questionTitle = question ^. title
  , _questionText = question ^. text
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

fromAnswerItemTemplatePlainWithIdsDTO :: AnswerItemTemplatePlainWithIdsDTO -> AnswerItemTemplatePlainWithIds
fromAnswerItemTemplatePlainWithIdsDTO itemTemplate =
  AnswerItemTemplatePlainWithIds
  { _answerItemTemplatePlainWithIdsTitle = itemTemplate ^. title
  , _answerItemTemplatePlainWithIdsQuestionIds = itemTemplate ^. questionIds
  }

fromExpertDTO :: ExpertDTO -> Expert
fromExpertDTO expert =
  Expert {_expertUuid = expert ^. uuid, _expertName = expert ^. name, _expertEmail = expert ^. email}

fromReferenceDTO :: ReferenceDTO -> Reference
fromReferenceDTO reference = Reference {_referenceUuid = reference ^. uuid, _referenceChapter = reference ^. chapter}
