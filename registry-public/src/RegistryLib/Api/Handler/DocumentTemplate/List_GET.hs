module RegistryLib.Api.Handler.DocumentTemplate.List_GET where

import Servant

import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleJM ()
import Shared.Common.Api.Handler.Common

type List_GET =
  Header "Authorization" String
    :> "document-templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "metamodelVersion" Int
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])

type Templates__List_GET =
  Header "Authorization" String
    :> "templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "metamodelVersion" Int
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DocumentTemplateSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
