module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Model.ActionKey.ActionKey
import Model.Config.DSWConfig

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''AppConfigClient

makeFields ''AppConfigWeb

makeFields ''AppConfigDatabase

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''BuildInfo

makeFields ''DSWConfig

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKeyDTO
makeFields ''ActionKeyDTO
