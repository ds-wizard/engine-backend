module Wizard.Api.Resource.Branch.Event.BranchEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Branch.Event.BranchEventDTO

instance ToJSON BranchEventDTO where
  toJSON = toSumJSON' "type"

instance FromJSON BranchEventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "AddBranchEvent" -> parseJSON (Object o) >>= \event -> return (AddBranchEventDTO' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON AddBranchEventDTO where
  parseJSON = simpleParseJSON "_addBranchEventDTO"

instance ToJSON AddBranchEventDTO where
  toJSON = simpleToJSON' "_addBranchEventDTO" "type"
