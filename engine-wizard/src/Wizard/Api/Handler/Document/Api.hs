module Wizard.Api.Handler.Document.Api where

import Servant

import Wizard.Api.Handler.Document.Detail_DELETE
import Wizard.Api.Handler.Document.Detail_Download_GET
import Wizard.Api.Handler.Document.List_GET
import Wizard.Api.Handler.Document.List_POST
import Wizard.Model.Context.BaseContext

type DocumentAPI
   = List_GET
     :<|> List_POST
     :<|> Detail_DELETE
     :<|> Detail_Download_GET

documentApi :: Proxy DocumentAPI
documentApi = Proxy

documentServer :: ServerT DocumentAPI BaseContextM
documentServer = list_GET :<|> list_POST :<|> detail_DELETE :<|> detail_download_GET
