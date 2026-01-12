module Wizard.Database.Migration.Development.Plugin.Data.PluginSettings where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Model.Plugin.Plugin

plugin1Dict :: M.Map U.UUID A.Value
plugin1Dict = M.fromList [(plugin1.uuid, plugin1Values1)]

plugin1Values1 :: A.Value
plugin1Values1 = A.Object $ KM.fromList [("setting1", A.String "value1"), ("setting2", A.Number 42)]

plugin1Values1Edited :: A.Value
plugin1Values1Edited = A.Object $ KM.fromList [("setting1Edited", A.String "value1"), ("setting2", A.Number 42)]

plugin1Values2 :: A.Value
plugin1Values2 = A.Object $ KM.fromList [("setting2", A.String "value2"), ("setting3", A.Number 44)]
