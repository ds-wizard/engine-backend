module Wizard.Api.Handler.Template.List_All_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_All_GET
   = Header "Authorization" String
     :> "templates"
     :> "all"
     :> QueryParam "organizationId" String
     :> QueryParam "templateId" String
     :> QueryParam "pkgId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])

list_all_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateSimpleDTO])
list_all_GET mTokenHeader mOrganizationId mTmlId mPkgId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organizationId" <$> mOrganizationId, (,) "templateId" <$> mTmlId]
      getTemplatesDto queryParams mPkgId
