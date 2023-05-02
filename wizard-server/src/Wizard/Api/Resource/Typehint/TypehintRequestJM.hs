module Wizard.Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import WizardLib.KnowledgeModel.Api.Resource.Event.EventJM ()

instance FromJSON TypehintRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypehintRequestDTO where
  toJSON = genericToJSON jsonOptions
