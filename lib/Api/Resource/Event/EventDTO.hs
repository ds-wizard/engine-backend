module Api.Resource.Event.EventDTO where

import Api.Resource.Event.AnswerEventDTO
import Api.Resource.Event.ChapterEventDTO
import Api.Resource.Event.ExpertEventDTO
import Api.Resource.Event.IntegrationEventDTO
import Api.Resource.Event.KnowledgeModelEventDTO
import Api.Resource.Event.QuestionEventDTO
import Api.Resource.Event.ReferenceEventDTO
import Api.Resource.Event.TagEventDTO

data EventDTO
  = AddKnowledgeModelEventDTO' AddKnowledgeModelEventDTO
  | EditKnowledgeModelEventDTO' EditKnowledgeModelEventDTO
  | AddChapterEventDTO' AddChapterEventDTO
  | EditChapterEventDTO' EditChapterEventDTO
  | DeleteChapterEventDTO' DeleteChapterEventDTO
  | AddQuestionEventDTO' AddQuestionEventDTO
  | EditQuestionEventDTO' EditQuestionEventDTO
  | DeleteQuestionEventDTO' DeleteQuestionEventDTO
  | AddAnswerEventDTO' AddAnswerEventDTO
  | EditAnswerEventDTO' EditAnswerEventDTO
  | DeleteAnswerEventDTO' DeleteAnswerEventDTO
  | AddExpertEventDTO' AddExpertEventDTO
  | EditExpertEventDTO' EditExpertEventDTO
  | DeleteExpertEventDTO' DeleteExpertEventDTO
  | AddReferenceEventDTO' AddReferenceEventDTO
  | EditReferenceEventDTO' EditReferenceEventDTO
  | DeleteReferenceEventDTO' DeleteReferenceEventDTO
  | AddTagEventDTO' AddTagEventDTO
  | EditTagEventDTO' EditTagEventDTO
  | DeleteTagEventDTO' DeleteTagEventDTO
  | AddIntegrationEventDTO' AddIntegrationEventDTO
  | EditIntegrationEventDTO' EditIntegrationEventDTO
  | DeleteIntegrationEventDTO' DeleteIntegrationEventDTO
  deriving (Show, Eq)
