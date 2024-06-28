module Wizard.Api.Handler.CommentThread.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.CommentThread.List_GET
import Wizard.Model.Context.BaseContext

type CommentThreadAPI =
  Tags "CommentThread"
    :> List_GET

commentThreadApi :: Proxy CommentThreadAPI
commentThreadApi = Proxy

commentThreadServer :: ServerT CommentThreadAPI BaseContextM
commentThreadServer =
  list_GET
