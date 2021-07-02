module Wizard.Api.Handler.User.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_GET
   = Header "Authorization" String
     :> "users"
     :> QueryParam "q" String
     :> QueryParam "role" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page UserDTO))

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page UserDTO))
list_GET mTokenHeader mQuery mRole mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getUsersPage mQuery mRole (Pageable mPage mSize) (parseSortQuery mSort)
