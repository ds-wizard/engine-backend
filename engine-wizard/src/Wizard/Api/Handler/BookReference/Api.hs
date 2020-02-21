module Wizard.Api.Handler.BookReference.Api where

import Servant

import Wizard.Api.Handler.BookReference.Detail_GET
import Wizard.Model.Context.BaseContext

type BookReferenceAPI = Detail_GET

bookReferenceApi :: Proxy BookReferenceAPI
bookReferenceApi = Proxy

bookReferenceServer :: ServerT BookReferenceAPI BaseContextM
bookReferenceServer = detail_GET
