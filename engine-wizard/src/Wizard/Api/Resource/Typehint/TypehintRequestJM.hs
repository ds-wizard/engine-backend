module Wizard.Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.Aeson
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

instance FromJSON TypehintRequestDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TypehintRequestDTO where
  toJSON = genericToJSON jsonOptions
