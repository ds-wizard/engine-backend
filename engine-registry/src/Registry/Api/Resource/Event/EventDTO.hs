module Registry.Api.Resource.Event.EventDTO where

import Registry.Api.Resource.Event.AnswerEventDTO
import Registry.Api.Resource.Event.ChapterEventDTO
import Registry.Api.Resource.Event.ExpertEventDTO
import Registry.Api.Resource.Event.IntegrationEventDTO
import Registry.Api.Resource.Event.KnowledgeModelEventDTO
import Registry.Api.Resource.Event.MoveEventDTO
import Registry.Api.Resource.Event.QuestionEventDTO
import Registry.Api.Resource.Event.ReferenceEventDTO
import Registry.Api.Resource.Event.TagEventDTO

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
  | MoveQuestionEventDTO' MoveQuestionEventDTO
  | MoveAnswerEventDTO' MoveAnswerEventDTO
  | MoveExpertEventDTO' MoveExpertEventDTO
  | MoveReferenceEventDTO' MoveReferenceEventDTO
  deriving (Show, Eq)
