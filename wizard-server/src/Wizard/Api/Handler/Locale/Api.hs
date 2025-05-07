module Wizard.Api.Handler.Locale.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Locale.Detail_Bundle_GET
import Wizard.Api.Handler.Locale.Detail_DELETE
import Wizard.Api.Handler.Locale.Detail_GET
import Wizard.Api.Handler.Locale.Detail_PUT
import Wizard.Api.Handler.Locale.Detail_Pull_POST
import Wizard.Api.Handler.Locale.List_Bundle_POST
import Wizard.Api.Handler.Locale.List_Current_Content_GET
import Wizard.Api.Handler.Locale.List_DELETE
import Wizard.Api.Handler.Locale.List_GET
import Wizard.Api.Handler.Locale.List_POST
import Wizard.Api.Handler.Locale.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type LocaleAPI =
  Tags "Locale"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> List_Current_Content_GET
          :<|> List_POST
          :<|> List_DELETE
          :<|> List_Bundle_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_Bundle_GET
          :<|> Detail_DELETE
          :<|> Detail_Pull_POST
       )

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: ServerT LocaleAPI BaseContextM
localeServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_current_content_GET
    :<|> list_POST
    :<|> list_DELETE
    :<|> list_bundle_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_bundle_GET
    :<|> detail_DELETE
    :<|> detail_pull_POST
