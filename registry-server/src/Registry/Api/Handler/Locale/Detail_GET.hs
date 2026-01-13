module Registry.Api.Handler.Locale.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Locale.LocaleDetailDTO
import Registry.Api.Resource.Locale.LocaleDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Locale.LocaleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate

type Detail_GET =
  "locales"
    :> Capture "id" Coordinate
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)

detail_GET :: Coordinate -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)
detail_GET coordinate = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getLocaleByCoordinate coordinate
