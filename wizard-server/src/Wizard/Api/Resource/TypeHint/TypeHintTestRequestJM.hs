module Wizard.Api.Resource.TypeHint.TypeHintTestRequestJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.TypeHint.TypeHintTestRequestDTO
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance FromJSON TypeHintTestRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintTestRequestDTO where
  toJSON = genericToJSON jsonOptions
