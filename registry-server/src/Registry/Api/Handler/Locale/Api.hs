module Registry.Api.Handler.Locale.Api where

import Servant

import Registry.Api.Handler.Locale.Detail_Bundle_GET
import Registry.Api.Handler.Locale.Detail_GET
import Registry.Api.Handler.Locale.List_Bundle_POST
import Registry.Api.Handler.Locale.List_GET
import Registry.Model.Context.BaseContext
import RegistryLib.Api.Handler.Locale.List_GET

type LocaleAPI =
  List_GET
    :<|> List_Bundle_POST
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: ServerT LocaleAPI BaseContextM
localeServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET
