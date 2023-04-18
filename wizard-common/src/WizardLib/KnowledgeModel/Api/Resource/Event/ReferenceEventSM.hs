module WizardLib.KnowledgeModel.Api.Resource.Event.ReferenceEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ReferenceEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Reference.ReferenceEvent

instance ToSchema AddReferenceEvent

instance ToSchema AddResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh1'

instance ToSchema AddURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh2'

instance ToSchema AddCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_rCh3'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditReferenceEvent

instance ToSchema EditResourcePageReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh1'

instance ToSchema EditURLReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh2'

instance ToSchema EditCrossReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_rCh3'

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_rCh2
