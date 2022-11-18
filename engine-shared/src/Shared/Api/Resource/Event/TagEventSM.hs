module Shared.Api.Resource.Event.TagEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.TagEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Tag.TagEvent
import Shared.Util.Swagger

instance ToSchema AddTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_tds

instance ToSchema EditTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_tds

instance ToSchema DeleteTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_tds
