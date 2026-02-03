module Wizard.Api.Resource.Plugin.PluginListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Plugin.PluginListJM ()
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Model.Plugin.PluginList

instance ToSchema PluginList where
  declareNamedSchema = toSwagger plugin1List
