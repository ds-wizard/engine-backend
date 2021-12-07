module Wizard.Api.Handler.Cache.List_KnowledgeModel_POST where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Cache.KnowledgeModelCache

type List_KnowledgeModel_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] KnowledgeModelChangeDTO
     :> "caches"
     :> "knowledge-model"
     :> Post '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] KnowledgeModel)

list_knowledgeModel_POST ::
     Maybe String
  -> Maybe String
  -> KnowledgeModelChangeDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] KnowledgeModel)
list_knowledgeModel_POST mServiceToken mServerUrl reqDto =
  runInUnauthService mServerUrl $
  addTraceUuidHeader =<< do
    checkServiceToken mServiceToken
    mKm <- getFromCache (reqDto ^. events) (reqDto ^. packageId) (reqDto ^. tagUuids)
    case mKm of
      Just km -> return km
      Nothing ->
        throwError $
        NotExistsError $
        _ERROR_DATABASE__ENTITY_NOT_FOUND
          "km-cache"
          [("key", cacheKey (fromMaybe "" $ reqDto ^. packageId) (reqDto ^. tagUuids))]
