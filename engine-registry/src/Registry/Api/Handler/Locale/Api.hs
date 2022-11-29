module Registry.Api.Handler.Locale.Api where

import Servant

import Registry.Api.Handler.Locale.Detail_Bundle_GET
import Registry.Api.Handler.Locale.Detail_GET
import Registry.Api.Handler.Locale.List_GET
import Registry.Model.Context.BaseContext

type LocaleAPI =
  List_GET
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: ServerT LocaleAPI BaseContextM
localeServer = list_GET :<|> detail_GET :<|> detail_bundle_GET
