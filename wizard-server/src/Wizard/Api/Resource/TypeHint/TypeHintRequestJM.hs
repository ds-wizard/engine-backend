module Wizard.Api.Resource.TypeHint.TypeHintRequestJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.TypeHint.TypeHintRequestDTO
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance FromJSON TypeHintRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypeHintRequestDTO where
  toJSON = genericToJSON jsonOptions
