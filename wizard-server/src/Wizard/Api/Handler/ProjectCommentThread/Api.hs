module Wizard.Api.Handler.ProjectCommentThread.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ProjectCommentThread.List_GET
import Wizard.Model.Context.BaseContext

type ProjectCommentThreadAPI =
  Tags "Project Comment Thread"
    :> List_GET

projectCommentThreadApi :: Proxy ProjectCommentThreadAPI
projectCommentThreadApi = Proxy

projectCommentThreadServer :: ServerT ProjectCommentThreadAPI BaseContextM
projectCommentThreadServer =
  list_GET
