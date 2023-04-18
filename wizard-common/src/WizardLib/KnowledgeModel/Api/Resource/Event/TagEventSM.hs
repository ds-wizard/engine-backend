module WizardLib.KnowledgeModel.Api.Resource.Event.TagEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.TagEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent

instance ToSchema AddTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_tds

instance ToSchema EditTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_tds

instance ToSchema DeleteTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_tds
