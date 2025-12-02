module Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent

instance ToSchema AddKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1

instance ToSchema EditKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1
