module Wizard.Api.Handler.User.Tour.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.User.Tour.Detail_PUT
import Wizard.Api.Handler.User.Tour.List_DELETE
import Wizard.Model.Context.BaseContext

type TourAPI =
  Tags "User Tour"
    :> ( List_DELETE
          :<|> Detail_PUT
       )

tourApi :: Proxy TourAPI
tourApi = Proxy

tourServer :: ServerT TourAPI BaseContextM
tourServer =
  list_DELETE
    :<|> detail_PUT
