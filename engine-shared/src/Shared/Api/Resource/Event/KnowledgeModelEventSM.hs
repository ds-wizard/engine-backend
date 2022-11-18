module Shared.Api.Resource.Event.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Swagger

instance ToSchema AddKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1

instance ToSchema EditKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1
