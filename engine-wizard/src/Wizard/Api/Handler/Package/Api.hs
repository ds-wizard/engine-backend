module Wizard.Api.Handler.Package.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Package.Detail_Bundle_GET
import Wizard.Api.Handler.Package.Detail_DELETE
import Wizard.Api.Handler.Package.Detail_GET
import Wizard.Api.Handler.Package.Detail_PUT
import Wizard.Api.Handler.Package.Detail_Pull_POST
import Wizard.Api.Handler.Package.List_Bundle_POST
import Wizard.Api.Handler.Package.List_DELETE
import Wizard.Api.Handler.Package.List_From_Branch_POST
import Wizard.Api.Handler.Package.List_From_Migration_POST
import Wizard.Api.Handler.Package.List_GET
import Wizard.Api.Handler.Package.List_POST
import Wizard.Api.Handler.Package.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type PackageAPI =
  Tags "Package"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> List_POST
          :<|> List_DELETE
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_DELETE
          :<|> List_Bundle_POST
          :<|> List_From_Branch_POST
          :<|> List_From_Migration_POST
          :<|> Detail_Bundle_GET
          :<|> Detail_Pull_POST
       )

packageApi :: Proxy PackageAPI
packageApi = Proxy

packageServer :: ServerT PackageAPI BaseContextM
packageServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> list_DELETE
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> list_bundle_POST
    :<|> list_from_branch_POST
    :<|> list_from_migration_POST
    :<|> detail_bundle_GET
    :<|> detail_pull_POST
