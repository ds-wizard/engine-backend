module Shared.Api.Resource.Event.EventDTO where

import GHC.Generics

import Shared.Api.Resource.Event.AnswerEventDTO
import Shared.Api.Resource.Event.ChapterEventDTO
import Shared.Api.Resource.Event.ExpertEventDTO
import Shared.Api.Resource.Event.IntegrationEventDTO
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Api.Resource.Event.MoveEventDTO
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.Event.ReferenceEventDTO
import Shared.Api.Resource.Event.TagEventDTO

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
  deriving (Show, Eq, Generic)
