module Wizard.Api.Handler.Branch.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Branch.Detail_DELETE
import Wizard.Api.Handler.Branch.Detail_GET
import Wizard.Api.Handler.Branch.Detail_PUT
import Wizard.Api.Handler.Branch.Detail_WS
import Wizard.Api.Handler.Branch.List_GET
import Wizard.Api.Handler.Branch.List_POST
import Wizard.Model.Context.BaseContext

type BranchAPI
   = Tags "Branch"
     :> (List_GET
         :<|> List_POST
         :<|> Detail_GET
         :<|> Detail_PUT
         :<|> Detail_DELETE
         :<|> Detail_WS)

branchApi :: Proxy BranchAPI
branchApi = Proxy

branchServer :: ServerT BranchAPI BaseContextM
branchServer = list_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|> detail_DELETE :<|> detail_WS
