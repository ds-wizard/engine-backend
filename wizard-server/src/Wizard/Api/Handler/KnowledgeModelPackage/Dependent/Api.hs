module Wizard.Api.Handler.KnowledgeModelPackage.Dependent.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.KnowledgeModelPackage.Dependent.List_GET
import Wizard.Model.Context.BaseContext

type DependentAPI =
  Tags "Knowledge Model Package Dependent"
    :> List_GET

dependentApi :: Proxy DependentAPI
dependentApi = Proxy

dependentServer :: ServerT DependentAPI BaseContextM
dependentServer =
  list_GET
