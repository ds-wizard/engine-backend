module Shared.Api.Resource.Event.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.KnowledgeModelEventDTO
import Shared.Api.Resource.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddKnowledgeModelEventDTO where
  declareNamedSchema = simpleToSchema'' "_addKnowledgeModelEventDTO" "eventType" (toDTO a_km1)

instance ToSchema EditKnowledgeModelEventDTO where
  declareNamedSchema = simpleToSchema'' "_editKnowledgeModelEventDTO" "eventType" (toDTO e_km1)
