module Wizard.Api.Handler.Project.Detail_Preview_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Detail.ProjectDetailPreviewJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Detail.ProjectDetailPreview
import Wizard.Service.Project.ProjectService

type Detail_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailPreview)

detail_preview_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDetailPreview)
detail_preview_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailPreviewById uuid
