module Wizard.Api.Handler.UserGroup.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.Group.UserGroupService
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailDTO
import WizardLib.Public.Api.Resource.User.Group.UserGroupDetailJM ()

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserGroupDetailDTO)

detail_GET :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserGroupDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUserGroupByUuid uuid
