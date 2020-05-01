module Shared.Api.Resource.Event.ReferenceEventSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventFieldSM ()
import Shared.Api.Resource.Event.EventJM ()
import Shared.Api.Resource.Event.ReferenceEventDTO
import Shared.Api.Resource.Event.ReferenceEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.Event.Data.Events
import Shared.Service.Event.EventToDTO
import Shared.Util.Swagger

instance ToSchema AddReferenceEventDTO

instance ToSchema AddResourcePageReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_addResourcePageReferenceEventDTO" "eventType" (toDTO a_km1_ch1_q2_rCh1')

instance ToSchema AddURLReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_addURLReferenceEventDTO" "eventType" (toDTO a_km1_ch1_q2_rCh2')

instance ToSchema AddCrossReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_addCrossReferenceEventDTO" "eventType" (toDTO a_km1_ch1_q2_rCh3')

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditReferenceEventDTO

instance ToSchema EditResourcePageReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_editResourcePageReferenceEventDTO" "eventType" (toDTO e_km1_ch1_q2_rCh1')

instance ToSchema EditURLReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_editURLReferenceEventDTO" "eventType" (toDTO e_km1_ch1_q2_rCh2')

instance ToSchema EditCrossReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_editCrossReferenceEventDTO" "eventType" (toDTO e_km1_ch1_q2_rCh3')

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteReferenceEventDTO where
  declareNamedSchema = simpleToSchema'' "_deleteReferenceEventDTO" "eventType" (toDTO d_km1_ch1_q2_rCh2)
