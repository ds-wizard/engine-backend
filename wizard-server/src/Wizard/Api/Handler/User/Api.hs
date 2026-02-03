module Wizard.Api.Handler.User.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.User.Detail_DELETE
import Wizard.Api.Handler.User.Detail_GET
import Wizard.Api.Handler.User.Detail_PUT
import Wizard.Api.Handler.User.Detail_Password_PUT
import Wizard.Api.Handler.User.Detail_State_PUT
import Wizard.Api.Handler.User.List_Current_GET
import Wizard.Api.Handler.User.List_Current_Locale_GET
import Wizard.Api.Handler.User.List_Current_Locale_PUT
import Wizard.Api.Handler.User.List_Current_PUT
import Wizard.Api.Handler.User.List_Current_Password_PUT
import Wizard.Api.Handler.User.List_Current_Submission_Props_GET
import Wizard.Api.Handler.User.List_Current_Submission_Props_PUT
import Wizard.Api.Handler.User.List_GET
import Wizard.Api.Handler.User.List_POST
import Wizard.Api.Handler.User.List_Suggestions_GET
import Wizard.Api.Handler.User.News.Api
import Wizard.Api.Handler.User.PluginSettings.Api
import Wizard.Api.Handler.User.Tour.Api
import Wizard.Model.Context.BaseContext

type UserAPI =
  Tags "User"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> List_POST
          :<|> List_Current_GET
          :<|> List_Current_PUT
          :<|> List_Current_Submission_Props_GET
          :<|> List_Current_Submission_Props_PUT
          :<|> List_Current_Password_PUT
          :<|> List_Current_Locale_GET
          :<|> List_Current_Locale_PUT
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_Password_PUT
          :<|> Detail_State_PUT
          :<|> Detail_DELETE
          :<|> NewsAPI
          :<|> PluginSettingsAPI
          :<|> TourAPI
       )

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: ServerT UserAPI BaseContextM
userServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> list_current_GET
    :<|> list_current_PUT
    :<|> list_current_submission_props_GET
    :<|> list_current_submission_props_PUT
    :<|> list_current_password_PUT
    :<|> list_current_locale_GET
    :<|> list_current_locale_PUT
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_password_PUT
    :<|> detail_state_PUT
    :<|> detail_DELETE
    :<|> newsServer
    :<|> pluginSettingsServer
    :<|> tourServer
