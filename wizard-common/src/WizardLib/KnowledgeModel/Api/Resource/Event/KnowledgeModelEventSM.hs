module WizardLib.KnowledgeModel.Api.Resource.Event.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.KnowledgeModelEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent

instance ToSchema AddKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1

instance ToSchema EditKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1
