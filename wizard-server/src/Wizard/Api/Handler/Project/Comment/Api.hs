module Wizard.Api.Handler.Project.Comment.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Comment.List_GET
import Wizard.Model.Context.BaseContext

type CommentAPI =
  Tags "Project Comment"
    :> List_GET

commentApi :: Proxy CommentAPI
commentApi = Proxy

commentServer :: ServerT CommentAPI BaseContextM
commentServer = list_GET
