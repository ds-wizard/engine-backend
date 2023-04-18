module Wizard.Api.Handler.ApiKey.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ApiKey.Detail_DELETE
import Wizard.Api.Handler.ApiKey.List_GET
import Wizard.Api.Handler.ApiKey.List_POST
import Wizard.Model.Context.BaseContext

type ApiKeyAPI =
  Tags "ApiKey"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_DELETE
       )

apiKeyApi :: Proxy ApiKeyAPI
apiKeyApi = Proxy

apiKeyServer :: ServerT ApiKeyAPI BaseContextM
apiKeyServer = list_GET :<|> list_POST :<|> detail_DELETE
