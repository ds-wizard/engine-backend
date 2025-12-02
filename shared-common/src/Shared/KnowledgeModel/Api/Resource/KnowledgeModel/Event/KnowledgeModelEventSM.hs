module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM where

import Data.Swagger

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Metric.MetricEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Move.MoveEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Question.QuestionEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.Tag.TagEventSM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent

instance ToSchema KnowledgeModelEvent where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema KnowledgeModelEventData where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
