module Wizard.Api.Handler.User.News.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.User.News.Detail_PUT
import Wizard.Model.Context.BaseContext

type NewsAPI =
  Tags "User News"
    :> Detail_PUT

newsApi :: Proxy NewsAPI
newsApi = Proxy

newsServer :: ServerT NewsAPI BaseContextM
newsServer = detail_PUT
