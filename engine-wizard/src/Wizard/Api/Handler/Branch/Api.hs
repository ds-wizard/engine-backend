module Wizard.Api.Handler.Branch.Api where

import Servant

import Wizard.Api.Handler.Branch.Detail_DELETE
import Wizard.Api.Handler.Branch.Detail_GET
import Wizard.Api.Handler.Branch.Detail_PUT
import Wizard.Api.Handler.Branch.List_GET
import Wizard.Api.Handler.Branch.List_POST
import Wizard.Api.Handler.Branch.List_Page_GET
import Wizard.Model.Context.BaseContext

type BranchAPI
   = List_GET
     :<|> List_Page_GET
     :<|> List_POST
     :<|> Detail_GET
     :<|> Detail_PUT
     :<|> Detail_DELETE

branchApi :: Proxy BranchAPI
branchApi = Proxy

branchServer :: ServerT BranchAPI BaseContextM
branchServer = list_GET :<|> list_page_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|> detail_DELETE
