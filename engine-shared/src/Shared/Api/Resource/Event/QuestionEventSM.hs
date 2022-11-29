module Shared.Api.Resource.Event.QuestionEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.QuestionEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Model.Event.Question.QuestionEvent
import Shared.Util.Swagger

instance ToSchema AddQuestionEvent

instance ToSchema AddOptionsQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2'

instance ToSchema AddMultiChoiceQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q11'

instance ToSchema AddListQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch2_q4'

instance ToSchema AddValueQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q1'

instance ToSchema AddIntegrationQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q9'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditQuestionEvent

instance ToSchema EditOptionsQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2'

instance ToSchema EditMultiChoiceQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q11'

instance ToSchema EditListQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch2_q4'

instance ToSchema EditValueQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q1'

instance ToSchema EditIntegrationQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q9'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q1
