module RegistryLib.Api.Handler.Locale.List_GET where

import Servant

import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Locale.LocaleJM ()
import Shared.Common.Api.Handler.Common

type List_GET =
  Header "Authorization" String
    :> "locales"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "recommendedAppVersion" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [LocaleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
