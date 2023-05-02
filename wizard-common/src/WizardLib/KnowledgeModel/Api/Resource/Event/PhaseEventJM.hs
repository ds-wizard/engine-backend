module WizardLib.KnowledgeModel.Api.Resource.Event.PhaseEventJM where

import Data.Aeson

import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.KnowledgeModel.Api.Resource.Event.EventFieldJM ()
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent

instance FromJSON AddPhaseEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddPhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditPhaseEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditPhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON DeletePhaseEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON DeletePhaseEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
