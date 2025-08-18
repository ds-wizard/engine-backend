module Wizard.Model.User.UserSubmissionPropEM where

import qualified Data.Map.Strict as M

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.Crypto (encryptAES256WithB64)
import Wizard.Model.User.UserSubmissionProp
import Wizard.Model.User.UserSubmissionPropList

instance SensitiveData UserSubmissionProp where
  process key entity =
    entity {values = M.map (encryptAES256WithB64 key) entity.values}

instance SensitiveData UserSubmissionPropList where
  process key entity =
    entity {values = M.map (encryptAES256WithB64 key) entity.values}
