module Shared.Api.Resource.Event.EventSM where

import Data.Swagger

import Shared.Api.Resource.Event.AnswerEventSM ()
import Shared.Api.Resource.Event.ChapterEventSM ()
import Shared.Api.Resource.Event.EventDTO
import Shared.Api.Resource.Event.EventFieldJM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.ExpertEventSM ()
import Shared.Api.Resource.Event.IntegrationEventSM ()
import Shared.Api.Resource.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.Event.MoveEventSM ()
import Shared.Api.Resource.Event.QuestionEventSM ()
import Shared.Api.Resource.Event.ReferenceEventSM ()
import Shared.Api.Resource.Event.TagEventSM ()

instance ToSchema EventDTO where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
