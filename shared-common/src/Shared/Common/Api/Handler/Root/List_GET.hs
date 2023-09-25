module Shared.Common.Api.Handler.Root.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.BaseContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.String

type List_GET = Get '[SafeJSON] (Headers '[] NoContent)

list_GET :: BaseContextC s sc m => String -> m (Headers '[] NoContent)
list_GET app = throwError =<< sendError (MovedPermanentlyError . f' "/%s-api" $ [app])
