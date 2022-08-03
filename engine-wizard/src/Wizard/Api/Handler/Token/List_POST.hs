module Wizard.Api.Handler.Token.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Api.Resource.Token.TokenCreateJM ()
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Api.Resource.Token.TokenJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Token.TokenService

type List_POST
   = Header "Host" String
     :> ReqBody '[ SafeJSON] TokenCreateDTO
     :> "tokens"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TokenDTO)

list_POST :: Maybe String -> TokenCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TokenDTO)
list_POST mServerUrl reqDto =
  runInUnauthService mServerUrl Transactional $ addTraceUuidHeader =<< generateTokenFromCredentials reqDto
