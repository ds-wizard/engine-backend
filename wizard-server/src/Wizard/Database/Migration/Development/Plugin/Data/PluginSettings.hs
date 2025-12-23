module Wizard.Database.Migration.Development.Plugin.Data.PluginSettings where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Util.Uuid

plugin1Dict :: M.Map U.UUID A.Value
plugin1Dict = M.fromList [(plugin1Uuid, plugin1Values1)]

plugin1Uuid :: U.UUID
plugin1Uuid = u' "13f297a0-d8b3-4efe-b65a-c78117dabad8"

plugin1Values1 :: A.Value
plugin1Values1 = A.Object $ KM.fromList [("setting1", A.String "value1"), ("setting2", A.Number 42)]

plugin1Values1Edited :: A.Value
plugin1Values1Edited = A.Object $ KM.fromList [("setting1Edited", A.String "value1"), ("setting2", A.Number 42)]

plugin1Values2 :: A.Value
plugin1Values2 = A.Object $ KM.fromList [("setting2", A.String "value2"), ("setting3", A.Number 44)]
