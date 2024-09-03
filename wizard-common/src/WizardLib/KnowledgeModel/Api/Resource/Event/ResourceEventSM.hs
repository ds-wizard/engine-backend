module WizardLib.KnowledgeModel.Api.Resource.Event.ResourceEventSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldSM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()
import WizardLib.KnowledgeModel.Api.Resource.Event.ResourceEventJM ()
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent

instance ToSchema AddResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1

instance ToSchema EditResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1

instance ToSchema DeleteResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1

-- --------------------------------------------
instance ToSchema AddResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1_rp1

instance ToSchema EditResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1_rp1

instance ToSchema DeleteResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1_rp1
