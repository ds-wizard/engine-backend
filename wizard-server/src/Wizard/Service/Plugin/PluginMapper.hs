module Wizard.Service.Plugin.PluginMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Plugin.Plugin

toPlugin :: U.UUID -> String -> U.UUID -> UTCTime -> Plugin
toPlugin uuid url tenantUuid now =
  Plugin
    { uuid = uuid
    , url = url
    , enabled = True
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }
