module Wizard.Api.Handler.BookReference.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.BookReference.BookReferenceDTO
import Wizard.Api.Resource.BookReference.BookReferenceJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.BookReference.BookReferenceService

type Detail_GET
   = "book-references"
     :> Capture "brShortUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] BookReferenceDTO)

detail_GET :: String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] BookReferenceDTO)
detail_GET brShortUuid = runInUnauthService $ addTraceUuidHeader =<< getBookReference brShortUuid
