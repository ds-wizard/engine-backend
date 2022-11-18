module Shared.Api.Resource.Event.ChoiceEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.ChoiceEventJM ()
import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Choice.ChoiceEvent
import Shared.Util.Swagger

instance ToSchema AddChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q11_cho1

instance ToSchema EditChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q11_cho1

instance ToSchema DeleteChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch3_q11_cho1
