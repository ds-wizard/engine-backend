module WizardLib.KnowledgeModel.Api.Resource.Event.EventSM where

import Data.Swagger

import WizardLib.KnowledgeModel.Api.Resource.Event.AnswerEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ChapterEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ChoiceEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ExpertEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.IntegrationEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.KnowledgeModelEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.MetricEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.MoveEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.PhaseEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.QuestionEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ReferenceEventSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.TagEventSM ()
import WizardLib.KnowledgeModel.Model.Event.Event

instance ToSchema Event where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
