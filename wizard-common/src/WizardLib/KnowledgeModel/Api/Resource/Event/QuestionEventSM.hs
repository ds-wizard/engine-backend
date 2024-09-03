module WizardLib.KnowledgeModel.Api.Resource.Event.QuestionEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.QuestionEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent

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

instance ToSchema AddItemSelectQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q13'

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

instance ToSchema EditItemSelectQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q13'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q1
