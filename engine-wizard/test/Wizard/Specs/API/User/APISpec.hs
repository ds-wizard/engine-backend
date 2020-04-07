module Wizard.Specs.API.User.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Detail_DELETE
import Wizard.Specs.API.User.Detail_Password_Hash_PUT
import Wizard.Specs.API.User.Detail_Password_PUT
import Wizard.Specs.API.User.List_Current_GET
import Wizard.Specs.API.User.List_Current_PUT
import Wizard.Specs.API.User.List_Current_Password_PUT
import qualified Wizard.Specs.API.UserAPISpec as Whole

userAPI appContext =
  with (startWebApp appContext) $
  describe "USER API Spec" $ do
    Whole.userAPI appContext
    list_current_GET appContext
    list_current_PUT appContext
    list_current_password_PUT appContext
    detail_password_put appContext
    detail_password_hash_put appContext
    detail_delete appContext
