module Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM where

import Data.Aeson

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

instance FromJSON KnowledgeModelMigrationState where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON KnowledgeModelMigrationState where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
