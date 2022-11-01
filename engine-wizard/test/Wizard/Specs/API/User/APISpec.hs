module Wizard.Specs.API.User.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Detail_DELETE
import Wizard.Specs.API.User.Detail_GET
import Wizard.Specs.API.User.Detail_PUT
import Wizard.Specs.API.User.Detail_Password_Hash_PUT
import Wizard.Specs.API.User.Detail_Password_PUT
import Wizard.Specs.API.User.Detail_State_PUT
import Wizard.Specs.API.User.List_Current_GET
import Wizard.Specs.API.User.List_Current_PUT
import Wizard.Specs.API.User.List_Current_Password_PUT
import Wizard.Specs.API.User.List_Current_Token_DELETE
import Wizard.Specs.API.User.List_GET
import Wizard.Specs.API.User.List_POST
import Wizard.Specs.API.User.List_Suggestions_GET

userAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
  describe "USER API Spec" $ do
    list_GET appContext
    list_suggestions_GET appContext
    list_POST appContext
    list_current_GET appContext
    list_current_PUT appContext
    list_current_password_PUT appContext
    list_current_token_DELETE appContext
    detail_GET appContext
    detail_PUT appContext
    detail_DELETE appContext
    detail_password_PUT appContext
    detail_password_hash_PUT appContext
    detail_state_PUT appContext
