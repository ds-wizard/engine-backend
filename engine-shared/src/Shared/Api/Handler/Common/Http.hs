module Shared.Api.Handler.Common.Http where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.UUID as U
import Servant (Header, Headers, addHeader)

import Shared.Model.Common.Sort
import Shared.Model.Context.ContextLenses
import Shared.Util.String (splitOn)

parseSortQuery :: Maybe String -> [Sort]
parseSortQuery Nothing = []
parseSortQuery (Just query) =
  case splitOn "," query of
    [by, "asc"] -> [Sort {_sortBy = by, _sortDirection = Ascending}]
    [by, "desc"] -> [Sort {_sortBy = by, _sortDirection = Descending}]
    _ -> []

addTraceUuidHeader ::
     (MonadReader s m, HasTraceUuid' s, MonadIO m) => a -> m (Headers '[ Header "x-trace-uuid" String] a)
addTraceUuidHeader result = do
  context <- ask
  let traceUuid = context ^. traceUuid'
  return $ addHeader (U.toString traceUuid) result
