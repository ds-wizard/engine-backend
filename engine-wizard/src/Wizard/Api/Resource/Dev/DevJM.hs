module Wizard.Api.Resource.Dev.DevJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Dev.Dev

instance FromJSON DevSection where
  parseJSON = simpleParseJSON "_devSection"

instance ToJSON DevSection where
  toJSON = simpleToJSON "_devSection"

instance FromJSON DevOperation where
  parseJSON = simpleParseJSON "_devOperation"

instance ToJSON DevOperation where
  toJSON = simpleToJSON "_devOperation"

instance FromJSON DevOperationParameter where
  parseJSON = simpleParseJSON "_devOperationParameter"

instance ToJSON DevOperationParameter where
  toJSON = simpleToJSON "_devOperationParameter"

instance FromJSON DevOperationParameterType

instance ToJSON DevOperationParameterType
