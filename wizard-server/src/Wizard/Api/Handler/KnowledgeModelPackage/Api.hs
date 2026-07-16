module Wizard.Api.Handler.KnowledgeModelPackage.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.KnowledgeModelPackage.Dependent.Api
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_DELETE
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_GET
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_Content_GET
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_DELETE
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_GET
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_POST
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Locales_Template_GET
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_PUT
import Wizard.Api.Handler.KnowledgeModelPackage.Detail_Pull_POST
import Wizard.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import Wizard.Api.Handler.KnowledgeModelPackage.List_From_Editor_POST
import Wizard.Api.Handler.KnowledgeModelPackage.List_From_Migration_POST
import Wizard.Api.Handler.KnowledgeModelPackage.List_GET
import Wizard.Api.Handler.KnowledgeModelPackage.List_POST
import Wizard.Api.Handler.KnowledgeModelPackage.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type KnowledgeModelPackageAPI =
  Tags "Knowledge Model Package"
    :> ( List_GET
           :<|> List_Suggestions_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> List_Bundle_POST
           :<|> List_From_Editor_POST
           :<|> List_From_Migration_POST
           :<|> Detail_Bundle_GET
           :<|> Detail_Pull_POST
           :<|> Detail_Locales_GET
           :<|> Detail_Locales_POST
           :<|> Detail_Locales_Template_GET
           :<|> Detail_Locales_Content_GET
           :<|> Detail_Locales_DELETE
           :<|> DependentAPI
       )

knowledgeModelPackageApi :: Proxy KnowledgeModelPackageAPI
knowledgeModelPackageApi = Proxy

knowledgeModelPackageServer :: ServerT KnowledgeModelPackageAPI BaseContextM
knowledgeModelPackageServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> list_bundle_POST
    :<|> list_from_editor_POST
    :<|> list_from_migration_POST
    :<|> detail_bundle_GET
    :<|> detail_pull_POST
    :<|> detail_locales_GET
    :<|> detail_locales_POST
    :<|> detail_locales_template_GET
    :<|> detail_locales_content_GET
    :<|> detail_locales_DELETE
    :<|> dependentServer
