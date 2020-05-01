module Shared.Api.Resource.Event.QuestionEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.QuestionEventDTO
import Shared.Api.Resource.Event.QuestionEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddQuestionEventDTO

instance ToSchema AddOptionsQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_addOptionsQuestionEventDTO" "eventType" (toDTO a_km1_ch1_q2')

instance ToSchema AddListQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_addListQuestionEventDTO" "eventType" (toDTO a_km1_ch2_q4')

instance ToSchema AddValueQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_addValueQuestionEventDTO" "eventType" (toDTO a_km1_ch1_q1')

instance ToSchema AddIntegrationQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_addIntegrationQuestionEventDTO" "eventType" (toDTO a_km1_ch3_q9')

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditQuestionEventDTO

instance ToSchema EditOptionsQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_editOptionsQuestionEventDTO" "eventType" (toDTO e_km1_ch1_q2')

instance ToSchema EditListQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_editListQuestionEventDTO" "eventType" (toDTO e_km1_ch2_q4')

instance ToSchema EditValueQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_editValueQuestionEventDTO" "eventType" (toDTO e_km1_ch1_q1')

instance ToSchema EditIntegrationQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_editIntegrationQuestionEventDTO" "eventType" (toDTO e_km1_ch3_q9')

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteQuestionEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteQuestionEventDTO" "eventType" (toDTO d_km1_ch1_q1)
