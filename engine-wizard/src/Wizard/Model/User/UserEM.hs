module Wizard.Model.User.UserEM where

import qualified Data.Map.Strict as M
import Shared.Util.Crypto (encryptAES256WithB64)
import Wizard.Model.Common.SensitiveData
import Wizard.Model.User.User

instance SensitiveData User where
  process key entity = entity {submissionProps = fmap (process key) (submissionProps entity)}

instance SensitiveData UserSubmissionProps where
  process key entity =
    entity {values = M.map (encryptAES256WithB64 key) (values entity)}
